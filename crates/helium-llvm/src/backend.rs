use helium_parser::ast::{Operator, AST};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{FloatValue, IntValue},
};

enum Eval<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
}

type MainFunc = unsafe extern "C" fn() -> i32;

fn interger_operation<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
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
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
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

pub fn main_fn<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    ast: AST,
) -> Option<JitFunction<'ctx, MainFunc>> {
    let i32_type = context.i32_type();
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_fn_type, None);
    let basic_block = context.append_basic_block(main_fn, "entry");

    let builder = context.create_builder();
    builder.position_at_end(basic_block);

    _ = evaluate(context, &builder, ast);

    builder.build_return(Some(&i32_type.const_zero()));
    unsafe { execution_engine.get_function("main").ok() }
}

fn evaluate<'ctx>(context: &'ctx Context, builder: &'ctx Builder<'ctx>, ast: AST) -> Eval<'ctx> {
    match ast {
        AST::Float(float_value) => {
            let f32_type = context.f32_type();
            Eval::Float(f32_type.const_float(float_value as f64))
        }
        AST::Integer(int_value) => {
            let i32_type = context.i32_type();
            Eval::Int(i32_type.const_int(int_value as u64, false))
        }
        AST::BinaryExpression { operator, lhs, rhs } => {
            let eval_lhs = evaluate(context, &builder, *lhs);
            let eval_rhs = evaluate(context, &builder, *rhs);

            match (eval_lhs, eval_rhs) {
                (Eval::Int(lhs), Eval::Int(rhs)) => {
                    Eval::Int(interger_operation(context, &builder, lhs, rhs, operator))
                }
                (Eval::Float(lhs), Eval::Float(rhs)) => {
                    Eval::Float(float_value_operation(context, &builder, lhs, rhs, operator))
                }
                _ => panic!("binary expression should contains same type operators"),
            }
        }
        _ => todo!("evaluate only supports binary operations with numbers"),
    }
}
