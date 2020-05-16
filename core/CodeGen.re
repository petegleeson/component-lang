open Llvm;
open Ast;

exception Error(string);

// let context = global_context();
// let the_module = create_module(context, "component_lang");
// let builder = builder(context);
// let int_type = i32_type(context);

// let rec gen_expression = expr => {
//   Expression.(
//     {
//       // print_endline(show_expression(expr));
//       switch (expr) {
//       | Int({raw}) => const_int(int_type, int_of_string(raw))
//       | BinaryOperator({operator, left, right}) =>
//         let left_val = gen_expression(left);
//         let right_val = gen_expression(right);
//         switch (operator) {
//         | Plus => build_add(left_val, right_val, "addtemp", builder)
//         | Minus => build_sub(left_val, right_val, "minustemp", builder)
//         | Multiply => build_mul(left_val, right_val, "multemp", builder)
//         | Divide => build_sdiv(left_val, right_val, "divtemp", builder)
//         };
//       | Function({params, body}) =>
//         let rec gen_last_expr = exprs =>
//           switch (exprs) {
//           | [] => None
//           | [Ast.Statement.Expression(expr)] => Some(gen_expression(expr))
//           | [expr, ...rest] => gen_last_expr(rest)
//           };

//         // declare fn
//         let fn =
//           declare_function("", function_type(int_type, [||]), the_module);

//         // generate body
//         let bb = append_block(context, "entry", fn);
//         // @Incomplete will only generate last expression in fn
//         let maybe_llval = gen_last_expr(body.expressions);
//         position_at_end(bb, builder);
//         switch (maybe_llval) {
//         | Some(llval) =>
//           build_ret(llval, builder);
//           ();
//         | None =>
//           build_ret_void(builder);
//           ();
//         };
//         fn;
//       | Block({expressions}) =>
//         let rec gen_last_expr = exprs =>
//           // @Incomplete will only generate last expression in fn
//           switch (exprs) {
//           | [Ast.Statement.Expression(expr)] => gen_expression(expr)
//           | [expr, ...rest] => gen_last_expr(rest)
//           };
//         gen_last_expr(expressions);
//       };
//     }
//   );
// }
// and gen_expressions = exprs =>
//   switch (exprs) {
//   | [] => None
//   | [expr] => Some([build_ret(gen_expression(expr), builder)])
//   | [expr, ...rest] =>
//     Some([
//       gen_expression(expr),
//       ...switch (gen_expressions(rest)) {
//          | None => []
//          | Some(vals) => vals
//          },
//     ])
//   }
// and gen_declaration = Ast.Statement.((Declaration.{id, value}) => {});

module Locals = Map.Make(String);

module GenEnv = {
  type t = {
    llcontext: Llvm.llcontext,
    llmodule: Llvm.llmodule,
    llbuilder: Llvm.llbuilder,
    lllocals: Locals.t(llvalue),
    scopes: list(Ast.Scope.t(Ast.Identifier.t)),
  };

  let set_lllocals = ({llcontext, llmodule, llbuilder, scopes}, lllocals) => {
    llcontext,
    llmodule,
    llbuilder,
    lllocals,
    scopes,
  };

  let is_global_scope = t => List.length(t.scopes) === 1;
};

let rec lltype_of = (env: GenEnv.t, kind) =>
  switch (kind) {
  | Int => i32_type(env.llcontext)
  | Func(params, ret) =>
    function_type(
      lltype_of(env, ret),
      Array.of_list(List.map(lltype_of(env), params)),
    )
  | x =>
    raise(
      Error(Printf.sprintf("cannot get lltype of %s", Ast.show_kind(x))),
    )
  };

let rec gen_expression =
  Expression.(
    (expr, env: GenEnv.t) =>
      switch (expr) {
      | BinaryOperator({operator, left, right}) =>
        let left_val = gen_expression(left, env);
        let right_val = gen_expression(right, env);
        switch (operator) {
        | Plus => build_add(left_val, right_val, "", env.llbuilder)
        | Minus => build_sub(left_val, right_val, "", env.llbuilder)
        | Multiply => build_mul(left_val, right_val, "", env.llbuilder)
        | Divide => build_sdiv(left_val, right_val, "", env.llbuilder)
        };
      | Int({kind, raw}) =>
        const_int(lltype_of(env, kind), int_of_string(raw))
      | Identifier({name}) =>
        switch (Locals.find_opt(name, env.lllocals)) {
        | Some(local) => local
        | None =>
          switch (lookup_global(name, env.llmodule)) {
          | Some(global) => global
          | None => raise(Error(Printf.sprintf("Cannot find id %s", name)))
          }
        }
      | Function({kind, params, body}) =>
        let fn = declare_function("", lltype_of(env, kind), env.llmodule);
        let locals =
          List.fold_left2(
            (locals, p: Identifier.t, llp) => {
              set_value_name(p.name, llp);
              Locals.add(p.name, llp, locals);
            },
            env.lllocals,
            params,
            Array.to_list(Llvm.params(fn)),
          );
        let bb = append_block(env.llcontext, "entry", fn);
        position_at_end(bb, env.llbuilder);
        let last_lval = gen_block(body, GenEnv.set_lllocals(env, locals));
        build_ret(last_lval, env.llbuilder);
        fn;
      | x =>
        raise(
          Error(
            Printf.sprintf(
              "cannot codegen expression %s",
              Ast.show_expression(x),
            ),
          ),
        )
      }
  )
and gen_block =
  Block.(
    ({expressions}, env) => {
      let rec gen_stmts = stmts =>
        switch (stmts) {
        | [] => raise(Error("can't code gen empty block"))
        | [stmt] => gen_statement(stmt, env)
        | [stmt, ...rest] =>
          gen_statement(stmt, env);
          gen_stmts(rest);
        };
      gen_stmts(expressions);
    }
  )
and gen_declaration =
  Declaration.(
    ({id, value}, env) =>
      switch (value, gen_expression(value, env)) {
      | (Function(_), lval) =>
        set_value_name(id.name, lval);
        lval;
      | (_, lval) when GenEnv.is_global_scope(env) =>
        define_global(id.name, lval, env.llmodule)
      | (_, lval) => lval
      }
  )
and gen_statement =
  Statement.(
    (stmt, env) => {
      switch (stmt) {
      | Expression(e) => gen_expression(e, env)
      | Declaration(d) => gen_declaration(d, env)
      };
    }
  );

let gen_program =
  Program.(
    (filename, program) => {
      let llcontext = global_context();
      let llmodule = create_module(llcontext, filename);
      let builder = Llvm.builder(llcontext);
      let env =
        GenEnv.{
          llcontext,
          llmodule,
          llbuilder: builder,
          lllocals: Locals.empty,
          scopes: [program.scope],
        };
      let rec gen_stmts = stmts => {
        switch (stmts) {
        | [] => ()
        | [stmt, ...rest] =>
          gen_statement(stmt, env);
          gen_stmts(rest);
        };
      };
      gen_stmts(program.body);
      string_of_llmodule(llmodule);
    }
  );

// this is sad for a number of reasons:
// - I can't figure out how to generate runnable WASM in one go
// - I can't generate any WASM without initializing all the backends (slow!)
// - I had to edit the install.sh file in llvm so the WASM backend was installed
let build_program =
  Program.(
    filename => {
      ()// //   },
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
        // Llvm_all_backends.initialize();
        // // Llvm_WebAssembly.initialize();
        // // print_endline(
        // //   switch (Llvm_target.Target.first()) {
        ; // //   | None => ""
 // //   | Some(target) => Llvm_target.Target.name(target)
        // );
    }
  );