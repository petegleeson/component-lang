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
          | [Ast.Statement.Expression(expr)] => Some(gen_expression(expr))
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
      | Block({expressions}) =>
        let rec gen_last_expr = exprs =>
          // @Incomplete will only generate last expression in fn
          switch (exprs) {
          | [Ast.Statement.Expression(expr)] => gen_expression(expr)
          | [expr, ...rest] => gen_last_expr(rest)
          };
        gen_last_expr(expressions);
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
        | [Ast.Statement.Expression(expr), ...rest] =>
          gen_expression(expr);
          gen_body(rest);
        };
      };
      gen_body(program.body);
    }
  );

// this is sad for a number of reasons:
// - I can't figure out how to generate runnable WASM in one go
// - I can't generate any WASM without initializing all the backends (slow!)
// - I had to edit the install.sh file in llvm so the WASM backend was installed
let build_program =
  Program.(
    filename => {
      ()// // Llvm_WebAssembly.initialize();
        // // print_endline(
        // //   switch (Llvm_target.Target.first()) {
        // //   | Some(target) => Llvm_target.Target.name(target)
        // //   | None => ""
        // //   },
        // // );
        // // List.iter(
        // //   x => print_endline(Llvm_target.Target.name(x)),
        // //   Llvm_target.Target.all(),
        // // );
        // let machine =
        //   Llvm_target.TargetMachine.create(
        //     ~triple="wasm32",
        //     Llvm_target.Target.by_triple("wasm32"),
        //   );
        // Llvm_target.TargetMachine.emit_to_file(
        //   the_module,
        //   AssemblyFile,
        //   filename,
        //   machine,
        ; // Llvm_all_backends.initialize();
        // );
    }
  );

let show_program = () => dump_module(the_module);
