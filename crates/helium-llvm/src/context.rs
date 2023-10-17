use std::{collections::HashMap, ops::Deref};

use helium_parser::ast::{Operator, Type, AST};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicValue, FloatValue, FunctionValue, InstructionValue, IntValue, PointerValue},
};

struct ContextBasicTypeEnum<'ctx>(BasicTypeEnum<'ctx>);

impl<'ctx> Into<Type> for ContextBasicTypeEnum<'ctx> {
    fn into(self) -> Type {
        match self.0 {
            BasicTypeEnum::IntType(_) => Type::I32,
            BasicTypeEnum::FloatType(_) => Type::F32,
            _ => Type::Void,
        }
    }
}

enum Value<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
}

pub struct FunctionContext<'a, 'ctx> {
    name: String,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    fn_value: Option<FunctionValue<'ctx>>,
    fn_args_types: Vec<BasicMetadataTypeEnum<'ctx>>,
    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx> FunctionContext<'a, 'ctx> {
    pub fn new(name: String, context: &'ctx Context, module: &'a Module<'ctx>) -> Self {
        FunctionContext {
            name,
            context,
            module,
            fn_args_types: vec![],
            fn_value: None,
            variables: HashMap::new(),
        }
    }
}

impl<'a, 'ctx> FunctionContext<'a, 'ctx> {
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

        self.fn_args_types = fn_args_types
    }

    pub fn with_return(&mut self, ret_type: Type) {
        let func = match ret_type {
            Type::I32 => {
                let i32_type = self.context.i32_type();
                let fn_type: inkwell::types::FunctionType<'_> =
                    i32_type.fn_type(&self.fn_args_types, false);
                self.module.add_function(self.name.as_str(), fn_type, None)
            }
            Type::F32 => {
                let f32_type = self.context.f32_type();
                let fn_type = f32_type.fn_type(&self.fn_args_types, false);
                self.module.add_function(self.name.as_str(), fn_type, None)
            }
            Type::Void => {
                let void_type = self.context.void_type();
                let fn_type = void_type.fn_type(&self.fn_args_types, false);
                self.module.add_function(self.name.as_str(), fn_type, None)
            }
        };

        self.fn_value = Some(func);
    }

    pub fn with_body(&mut self, body: Vec<Box<AST>>) {
        let fn_value = self.fn_value.unwrap();
        let basic_block = self.context.append_basic_block(fn_value, "entry");

        let builder = self.context.create_builder();
        builder.position_at_end(basic_block);

        self.evaluate_stmts(&builder, body);
    }

    fn evaluate_stmts(&mut self, builder: &Builder<'ctx>, stmts: Vec<Box<AST>>) {
        let stmts = stmts.into_iter().map(|boxed| *boxed);
        for stmt in stmts {
            match stmt {
                AST::LetStatment {
                    variable,
                    rhs,
                    var_type,
                } => {
                    let expression = self.evaluate_inner(builder, *rhs, var_type);

                    let pointer_value = match var_type {
                        Type::I32 => builder
                            .build_alloca(self.context.i32_type(), variable.as_str())
                            .unwrap(),
                        Type::F32 => builder
                            .build_alloca(self.context.f32_type(), variable.as_str())
                            .unwrap(),
                        Type::Void => continue,
                    };

                    match expression {
                        Value::Int(value) => builder.build_store(pointer_value, value).unwrap(),
                        Value::Float(value) => builder.build_store(pointer_value, value).unwrap(),
                    };
                    self.variables.insert(variable, pointer_value);
                }
                AST::ReturnStatement(expr) => {
                    let fn_type: Type = if let Some(fn_type) =
                        self.fn_value.unwrap().get_type().get_return_type()
                    {
                        ContextBasicTypeEnum(fn_type).into()
                    } else {
                        Type::Void
                    };

                    let return_value = self.evaluate_inner(builder, *expr, fn_type);

                    match fn_type {
                        Type::I32 => {
                            match return_value {
                                Value::Int(int_value) => builder.build_return(Some(&int_value)),
                                _ => panic!("expected type integer"),
                            };
                        }
                        Type::F32 => {
                            match return_value {
                                Value::Float(float_value) => {
                                    builder.build_return(Some(&float_value))
                                }
                                _ => panic!("expected type float"),
                            };
                        }
                        _ => panic!("return statement not implemented yet"),
                    };
                }
                _ => {}
            }
        }
    }

    fn evaluate_inner(&mut self, builder: &Builder<'ctx>, ast: AST, ttype: Type) -> Value<'ctx> {
        match ast {
            AST::Float(float_value) => {
                let f32_type = self.context.f32_type();
                Value::Float(f32_type.const_float(float_value as f64))
            }
            AST::Integer(int_value) => {
                let i32_type = self.context.i32_type();
                Value::Int(i32_type.const_int(int_value as u64, false))
            }
            AST::Identifier(identifier) => match ttype {
                Type::I32 => {
                    let int_value = builder
                        .build_load(
                            self.context.i32_type(),
                            *self.variables.get(&identifier).unwrap(),
                            identifier.as_str(),
                        )
                        .unwrap()
                        .into_int_value();
                    Value::Int(int_value)
                }
                Type::F32 => {
                    let float_value = builder
                        .build_load(
                            self.context.f32_type(),
                            *self.variables.get(&identifier).unwrap(),
                            identifier.as_str(),
                        )
                        .unwrap()
                        .into_float_value();
                    Value::Float(float_value)
                }
                _ => panic!("expected ttype"),
            },
            AST::BinaryExpression { operator, lhs, rhs } => {
                let eval_lhs = self.evaluate_inner(builder, *lhs, ttype);
                let eval_rhs = self.evaluate_inner(builder, *rhs, ttype);

                match (eval_lhs, eval_rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(interger_operation(
                        self.context,
                        builder,
                        lhs,
                        rhs,
                        operator,
                    )),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(float_value_operation(
                        self.context,
                        builder,
                        lhs,
                        rhs,
                        operator,
                    )),
                    _ => panic!("binary expression should contains same type operators"),
                }
            }
            _ => todo!("evaluate_inner not completed"),
        }
    }
}

fn interger_operation<'ctx>(
    context: &Context,
    builder: &Builder<'ctx>,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    op: Operator,
) -> IntValue<'ctx> {
    let i32_type = context.i32_type();
    match op {
        Operator::Plus => builder.build_int_add(lhs, rhs, "sum").unwrap(),
        _ => todo!("integer operation only supports sum"),
    }
}

fn float_value_operation<'ctx>(
    context: &Context,
    builder: &Builder<'ctx>,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    op: Operator,
) -> FloatValue<'ctx> {
    let i32_type = context.i32_type();
    match op {
        Operator::Plus => builder.build_float_add(lhs, rhs, "sum").unwrap(),
        _ => todo!("integer operation only supports sum"),
    }
}
