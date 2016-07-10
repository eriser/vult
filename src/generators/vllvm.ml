(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

open CLike
open Llvm

module Vars = Map.Make(String)

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context

let cfloat_type = float_type context
let cinteger_type = i32_type context
let cbool_type = i1_type context

let n = const_int cinteger_type 0 

(* Create a new basic block to start insertion into. *)
 let ft = function_type cbool_type [||] ;;
let the_function = declare_function "kk" ft the_module ;;
let bb = append_block context "entry" the_function ;;
position_at_end bb builder;;


let rec exp (ctx:llvalue Vars.t) (e:cexp) : llvalue =
   match e with
   | CEInt(n) -> const_int cinteger_type n
   | CEFloat(s,_) -> const_float_of_string cfloat_type s
   | CEBool(b) -> const_int cbool_type (if b then 1 else 0)
   | CEString(s) -> const_string context s
   | CEVar(v) -> Vars.find v ctx
   | CEIf(cond,then_,else_) ->
      let cond_val = exp ctx cond in
      (* Grab the first block so that we might later add the conditional branch
       * to it at the end of the function. *)
      let start_bb = insertion_block builder in (* [insertion_block b] returns the basic block that the builder [b] is positioned to insert into.*)
      let the_function = block_parent start_bb in (* [block_parent bb] returns the parent function that owns the basic block. *)

      let then_bb = append_block context "then" the_function in (* [append_block c name f] creates a new basic block named [name] at the end of function [f] in the context [c].*)

      (* Emit 'then' value. *)
      position_at_end then_bb builder; (* [position_at_end bb b] moves the instruction builder [b] to the end of the basic block [bb]. *)
      let then_val = exp ctx then_ in
      (* Codegen of 'then' can change the current block, update then_bb for the
       * phi. We create a new name because one is used for the phi node, and the
       * other is used for the conditional branch. *)
      let new_then_bb = insertion_block builder in

      (* Emit 'else' value. *)
      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let else_val = exp ctx else_ in

      (* Codegen of 'else' can change the current block, update else_bb for the
       * phi. *)
      let new_else_bb = insertion_block builder in

      (* Emit merge block. *)
      let merge_bb = append_block context "ifcont" the_function in
      position_at_end merge_bb builder;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" builder in

      (* Return to the start block to add the conditional branch. *)
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);

      (* Set a unconditional branch at the end of the 'then' block and the
       * 'else' block to the 'merge' block. *)
      position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

      (* Finally, set the builder to the end of the merge block. *)
      position_at_end merge_bb builder;

      phi

   | _ -> failwith ""


let run () =
   let _ =
      CEIf(CEBool(false),CEIf(CEBool(true),CEFloat("0.0",0.0),CEFloat("1.0",1.0)),CEFloat("2.0",2.0))
      |> exp Vars.empty
   in
   dump_value the_function