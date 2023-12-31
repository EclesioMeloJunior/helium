#![allow(unused)]
pub mod backend;
pub mod context;

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
    fn simple_variable_sum() {
        let programs: Vec<String> = vec![
            String::from(
                "func main() : i32 { 
                    let c : i32 = 1 + 1;
                    let b : i32 = c + 1;
                    return b
                }",
            ),
            String::from(
                "func main() : i32 { 
                    let c : f32 = 1.0 + 1.20;
                    let b : f32 = c + 1.40;
                    return 0
                }",
            ),
        ];

        for program in programs {
            let context = Context::create();
            let module = context.create_module("main_helium");
            let execution_engine = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            let lexer = Lexer::from(program);
            let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();

            let mut parser = Parser::new(lexer);
            let ast = parser.program().unwrap();

            println!("{ast:?}");

            let mut generator = backend::Generator::new(&context, module, execution_engine);

            let main_fn = generator.generate(ast).unwrap();

            unsafe {
                println!("{}", main_fn.call());
            }
        }
    }
}
