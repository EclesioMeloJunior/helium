#![allow(unused)]

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace,
};

type SumFunc = unsafe extern "C" fn(u64, u64) -> u64;
type MainFunc = unsafe extern "C" fn() -> u64;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compiler_sum(&self) {
        let i64_t = self.context.i64_type();
        let fn_t = i64_t.fn_type(&[i64_t.into(), i64_t.into()], false);
        let sum_fn: inkwell::values::FunctionValue<'_> =
            self.module.add_function("sum", fn_t, None);
        let basic_block = self.context.append_basic_block(sum_fn, "entry");

        self.builder.position_at_end(basic_block);

        let x = sum_fn.get_nth_param(0).unwrap().into_int_value();
        let y = sum_fn.get_nth_param(1).unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        self.builder.build_return(Some(&sum)).unwrap();

        let main_fn_t = i64_t.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_t, None);
        let main_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(main_block);

        let args = [
            BasicMetadataValueEnum::IntValue(i64_t.const_int(5, false)),
            BasicMetadataValueEnum::IntValue(i64_t.const_int(7, false)),
        ];

        let result = self
            .builder
            .build_call(sum_fn, &args, "add_result")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        let i8_t = self.context.i8_type();
        let printf_t = i8_t.fn_type(&[i8_t.ptr_type(AddressSpace::default()).into()], true);
        let printf_fn = self.module.add_function("printf", printf_t, None);
        let global_format_str = self
            .builder
            .build_global_string_ptr("The sum is: %d\n", "fmt")
            .unwrap();

        self.builder.build_call(
            printf_fn,
            &[
                BasicMetadataValueEnum::PointerValue(global_format_str.as_pointer_value()),
                BasicMetadataValueEnum::IntValue(result.into_int_value()),
            ],
            "printf_call",
        );

        self.builder.build_return(Some(&i64_t.const_zero()));
        self.module.print_to_file("output.ll");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn code_gen_simple_sum() {
        let context = Context::create();
        let module = context.create_module("sum");
        let execution_engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        let codegen = CodeGen {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
        };

        codegen.jit_compiler_sum();
    }
}
