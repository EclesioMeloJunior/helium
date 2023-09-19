#![allow(unused)]
pub mod backend;

use helium_parser::ast::{Operator, AST};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace,
};

#[cfg(test)]
mod tests {
    use super::*;
    use helium_lexer::{self, Lexer};
    use helium_parser::{self, Parser};
    use inkwell::OptimizationLevel;

    #[test]
    fn code_gen_simple_sum() {
        let context = Context::create();
        let module = context.create_module("main_helium");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let program = String::from(
            "func main() {
            return 1 + 1
        }",
        );
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();

        let mut parser = Parser::new(lexer);
        let ast = parser.program().unwrap();

        let main_fn = backend::main_fn(&context, &module, execution_engine, ast).unwrap();
        module.print_to_file("output.ll");

        unsafe {
            println!("{}", main_fn.call());
        }
    }
}
