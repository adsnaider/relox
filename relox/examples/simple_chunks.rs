use relox::lox::{
    chunk::{Chunk, Instr},
    value::Value,
    vm::Vm,
};

fn main() {
    env_logger::init();
    let mut chunk = Chunk::new();
    let idx = chunk.add_constant(Value(1.2));
    chunk.add_instruction(Instr::Const(idx), 123);
    let idx = chunk.add_constant(Value(3.4));
    chunk.add_instruction(Instr::Const(idx), 123);
    chunk.add_instruction(Instr::Add, 123);
    let idx = chunk.add_constant(Value(5.6));
    chunk.add_instruction(Instr::Const(idx), 123);
    chunk.add_instruction(Instr::Div, 123);
    chunk.add_instruction(Instr::Negate, 123);
    chunk.add_instruction(Instr::Return, 123);

    let mut vm = Vm::new();
    vm.interpret(chunk).unwrap();
}
