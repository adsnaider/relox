use std::{
    alloc::Layout, borrow::Borrow, clone::CloneToUninit, fmt::Display, ops::Deref, ptr::NonNull,
};

use derive_more::derive::{Deref, Display};
use hashbrown::HashSet;

impl From<Gc<str>> for GcObj {
    fn from(value: Gc<str>) -> Self {
        Self::Str(value)
    }
}

#[derive(Debug)]
pub enum GcObj {
    Str(Gc<str>),
}

#[derive(Debug)]
pub struct Gc<T: ?Sized> {
    value: NonNull<GcData<T>>,
}

unsafe impl<T: ?Sized + Sync> Sync for Gc<T> {}
unsafe impl<T: ?Sized + Sync> Send for Gc<T> {}

impl<T: ?Sized + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self { value: self.value }
    }
}
impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data() == other.data()
    }
}
impl<T: ?Sized + Eq> Eq for Gc<T> {}
impl<T: ?Sized + std::hash::Hash> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data().hash(state)
    }
}

#[repr(C)]
pub struct GcData<T: ?Sized> {
    next: Option<GcObj>,
    data: T,
}

impl<T: ?Sized> Gc<T> {
    pub fn data(&self) -> &T {
        &*self
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.value.as_ref().data }
    }
}

impl<T: ?Sized> Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        self.data()
    }
}

#[derive(Debug)]
pub struct Heap {
    objects: GcObjects,
    strings: HashSet<Gc<str>>,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Display, Clone, Deref)]
pub struct InternedStr(Gc<str>);

impl PartialEq for InternedStr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0.value.as_ptr(), other.0.value.as_ptr())
    }
}

#[derive(Debug)]
pub struct GcObjects {
    head: Option<GcObj>,
}

impl GcObjects {
    pub fn new() -> Self {
        Self { head: None }
    }

    pub fn push<T: ?Sized + CloneToUninit>(&mut self, value: &T) -> Gc<T>
    where
        Gc<T>: Into<GcObj>,
    {
        let (layout, data_offset) = {
            let start_layout = Layout::new::<Option<GcObj>>();
            let (layout, data_offset) = start_layout.extend(Layout::for_value(value)).unwrap();
            let layout = layout.pad_to_align();
            (layout, data_offset)
        };
        let allocation = unsafe { std::alloc::alloc(layout) };
        unsafe {
            std::ptr::write(allocation as *mut Option<GcObj>, None);
            value.clone_to_uninit(allocation.offset(data_offset as isize));
        }

        let node = std::ptr::from_raw_parts_mut::<GcData<T>>(
            allocation as *mut (),
            std::ptr::metadata(value),
        );

        let mut node = NonNull::new(node).unwrap();
        {
            let node = unsafe { node.as_mut() };
            node.next = self.head.take();
        }
        let pointer = Gc { value: node };
        self.head = Some(pointer.clone().into());
        pointer
    }

    unsafe fn pop(&mut self) -> Option<()> {
        let head = self.head.take()?;
        let (node, layout, next): (NonNull<u8>, _, _) = match head {
            GcObj::Str(mut gc) => (
                gc.value.cast(),
                Layout::for_value(gc.value.as_ref()),
                unsafe { gc.value.as_mut().next.take() },
            ),
        };
        self.head = next;
        std::alloc::dealloc(node.as_ptr(), layout);
        Some(())
    }

    pub unsafe fn drain(&mut self) {
        while let Some(()) = unsafe { self.pop() } {}
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: GcObjects::new(),
            strings: HashSet::new(),
        }
    }
    pub fn push<T: ?Sized + CloneToUninit>(&mut self, value: &T) -> Gc<T>
    where
        Gc<T>: Into<GcObj>,
    {
        self.objects.push(value)
    }

    pub fn push_interned(&mut self, value: &str) -> InternedStr {
        let value = self
            .strings
            .get_or_insert_with(value, |s| self.objects.push(s));
        InternedStr(value.clone())
    }

    pub unsafe fn drain(&mut self) {
        unsafe { self.objects.drain() };
        drop(core::mem::take(&mut self.strings));
    }
}
