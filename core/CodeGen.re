open Llvm;

exception Error(string);

let context = global_context();
let the_module = create_module(context, "my cool jit");
let builder = builder(context);