open Llvm;
open Ast;

exception Error(string);

let context = global_context();
let the_module = create_module(context, "component_lang");
let builder = builder(context);
let int_type = i32_type(context);

let rec gen_expression = expr => {
  Expression.(
    {
      // print_endline(show_expression(expr));
      switch (expr) {
      | Int({raw}) => const_int(int_type, int_of_string(raw))
      | BinaryOperator({operator, left, right}) =>
        let left_val = gen_expression(left);
        let right_val = gen_expression(right);
        switch (operator) {
        | Plus => build_add(left_val, right_val, "addtemp", builder)
        | Minus => build_sub(left_val, right_val, "minustemp", builder)
        | Multiply => build_mul(left_val, right_val, "multemp", builder)
        | Divide => build_sdiv(left_val, right_val, "divtemp", builder)
        };
      | Function({params, body}) =>
        let rec gen_last_expr = exprs =>
          switch (exprs) {
          | [] => None
          | [expr] => Some(gen_expression(expr))
          | [expr, ...rest] => gen_last_expr(rest)
          };

        // declare fn
        let fn =
          declare_function("", function_type(int_type, [||]), the_module);

        // generate body
        let bb = append_block(context, "entry", fn);
        // @Incomplete will only generate last expression in fn
        let maybe_llval = gen_last_expr(body.expressions);
        position_at_end(bb, builder);
        switch (maybe_llval) {
        | Some(llval) =>
          build_ret(llval, builder);
          ();
        | None =>
          build_ret_void(builder);
          ();
        };
        fn;
      | _ => raise(Error("don't know how to code gen this"))
      };
    }
  );
}
and gen_expressions = exprs =>
  switch (exprs) {
  | [] => None
  | [expr] => Some([build_ret(gen_expression(expr), builder)])
  | [expr, ...rest] =>
    Some([
      gen_expression(expr),
      ...switch (gen_expressions(rest)) {
         | None => []
         | Some(vals) => vals
         },
    ])
  };

let gen_program =
  Program.(
    program => {
      let rec gen_body = exprs => {
        switch (exprs) {
        | [] => ()
        | [expr, ...rest] =>
          gen_expression(expr);
          gen_body(rest);
        };
      };
      gen_body(program.body);
    }
  );

let show_program = () => dump_module(the_module);