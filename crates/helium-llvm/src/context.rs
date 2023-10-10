use helium_parser::ast::Type;
use inkwell::{
    context::Context, module::Module, types::BasicMetadataTypeEnum, values::FunctionValue,
};

pub struct FunctionContext<'ctx> {
    name: &'ctx str,
    context: &'ctx Context,
    module: Module<'ctx>,
    fn_value: Option<FunctionValue<'ctx>>,
    fn_args_types: &'ctx [BasicMetadataTypeEnum<'ctx>],
}

pub struct FunctionBody();

impl<'ctx> FunctionContext<'ctx> {
    pub fn new(name: &'ctx str, context: &'ctx Context, module: Module<'ctx>) -> Self {
        FunctionContext {
            name,
            context,
            module,
            fn_args_types: &vec![],
            fn_value: None,
        }
    }
}

impl<'ctx> FunctionContext<'ctx> {
    pub fn with_argument_types(&mut self, arg_types: Vec<Type>) {
        let mut fn_args_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

        for arg_type in arg_types {
            match arg_type {
                Type::I32 => {
                    let i32_type = self.context.i32_type();
                    fn_args_types.push(BasicMetadataTypeEnum::IntType(i32_type));
                }
                Type::F32 => {
                    let f32_type = self.context.f32_type();
                    fn_args_types.push(BasicMetadataTypeEnum::FloatType(f32_type));
                }
                _ => continue,
            }
        }

        self.fn_args_types = &fn_args_types
    }

    pub fn with_return(&mut self, ret_type: Type) {
        let func = match ret_type {
            Type::I32 => {
                let i32_type = self.context.i32_type();
                let fn_type = i32_type.fn_type(self.fn_args_types, false);
                self.module.add_function(self.name, fn_type, None)
            }
            Type::F32 => {
                let f32_type = self.context.f32_type();
                let fn_type = f32_type.fn_type(self.fn_args_types, false);
                self.module.add_function(self.name, fn_type, None)
            }
            Type::Void => {
                let void_type = self.context.void_type();
                let fn_type = void_type.fn_type(self.fn_args_types, false);
                self.module.add_function(self.name, fn_type, None)
            }
        };

        self.fn_value = Some(func);
    }
}
