use crate::context::FunctionContext;
use helium_parser::ast::{Operator, AST};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{FloatValue, IntValue, PointerValue},
};
use std::collections::HashMap;

enum Eval<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
}

type MainFunc = unsafe extern "C" fn() -> i32;

pub struct Generator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Generator<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Generator {
            context,
            module,
            execution_engine,
            variables: HashMap::new(),
        }
    }

    pub fn generate(&mut self, ast: AST) -> Option<JitFunction<'ctx, MainFunc>> {
        match ast {
            AST::FunctionLiteral {
                name,
                args,
                body,
                return_type,
            } => {
                let fn_name = name.clone();
                let mut function_ctx = FunctionContext::new(fn_name, self.context, &self.module);

                function_ctx.with_argument_types(args.into_iter().map(|item| item.1).collect());
                function_ctx.with_return(return_type);
                function_ctx.with_body(body);

                //self.module.print_to_file("output.ll");

                unsafe { self.execution_engine.get_function(name.as_str()).ok() }
            }
            _ => return None,
        }
    }

    fn evaluate_stmts(&mut self, builder: &Builder<'ctx>, stmts: Vec<Box<AST>>) {
        for stmt in stmts {
            match *stmt {
                AST::LetStatment {
                    variable,
                    rhs,
                    var_type,
                } => {
                    let expression = self.evaluate_inner(builder, *rhs);
                    let i32_type = self.context.i32_type();
                    let p_value = builder.build_alloca(i32_type, variable.as_str()).unwrap();

                    match expression {
                        Eval::Int(i_value) => builder.build_store(p_value, i_value).unwrap(),
                        _ => todo!("cannot store other types"),
                    };

                    self.variables.insert(variable, p_value);
                }
                AST::ReturnStatement(expr) => match *expr {
                    AST::Identifier(n) => {
                        let loaded_value = builder
                            .build_load(
                                self.context.i32_type(),
                                *self.variables.get(&n).unwrap(),
                                n.as_str(),
                            )
                            .unwrap()
                            .into_int_value();

                        builder.build_return(Some(&loaded_value));
                    }
                    _ => todo!("cannot parse in the return {:?}", *expr),
                },
                _ => todo!("cannot eval statement{:?}", *stmt),
            }
        }
    }

    fn evaluate_inner(&mut self, builder: &Builder<'ctx>, ast: AST) -> Eval<'ctx> {
        match ast {
            AST::Float(float_value) => {
                let f32_type = self.context.f32_type();
                Eval::Float(f32_type.const_float(float_value as f64))
            }
            AST::Integer(int_value) => {
                let i32_type = self.context.i32_type();
                Eval::Int(i32_type.const_int(int_value as u64, false))
            }
            AST::Identifier(identifier) => {
                let loaded_value = builder
                    .build_load(
                        self.context.i32_type(),
                        *self.variables.get(&identifier).unwrap(),
                        identifier.as_str(),
                    )
                    .unwrap()
                    .into_int_value();

                Eval::Int(loaded_value)
            }
            AST::BinaryExpression { operator, lhs, rhs } => {
                let eval_lhs = self.evaluate_inner(&builder, *lhs);
                let eval_rhs = self.evaluate_inner(&builder, *rhs);

                match (eval_lhs, eval_rhs) {
                    (Eval::Int(lhs), Eval::Int(rhs)) => Eval::Int(interger_operation(
                        self.context,
                        &builder,
                        lhs,
                        rhs,
                        operator,
                    )),
                    (Eval::Float(lhs), Eval::Float(rhs)) => Eval::Float(float_value_operation(
                        self.context,
                        &builder,
                        lhs,
                        rhs,
                        operator,
                    )),
                    _ => panic!("binary expression should contains same type operators"),
                }
            }
            _ => todo!("evaluate_inner"),
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
