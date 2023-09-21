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
    use crate::backend::Generator;

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
                let a = 1 + 1;
                return a
            }",
        );
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();

        let mut parser = Parser::new(lexer);
        let ast = parser.program().unwrap();

        println!("{ast:?}");

        let mut generator = backend::Generator::new(&context, module, execution_engine);

        let main_fn = generator.generate(ast).unwrap();

        // unsafe {
        //     println!("{}", main_fn.call());
        // }
    }
}
