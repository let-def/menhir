(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(**************************************************************************)

open EngineTypes

(* The LR parsing engine. *)

let fst3 (a,_,_) = a
let snd3 (_,a,_) = a
let thd3 (_,_,a) = a

(* This module is used:

   - at compile time, if so requested by the user, via the --interpret options;
   - at run time, in the table-based back-end. *)

module Make (T : TABLE) = struct

  (* This propagates type and exception definitions. *)

  include T

  let _eRR : exn =
    Error

  (* --------------------------------------------------------------------------- *)

  (* Alias to [EngineTypes.env] specialized to current definitions. *)

  type env' = (state, semantic_value, token) env

  type step =
    | Step_run    of env'
    | Step_error  of env'
    | Step_action of env'

  type parser =
    | Step   of step
    | Accept of semantic_value
    | Reject
    | Feed   of (Lexing.position * token * Lexing.position -> step)

  (* --------------------------------------------------------------------------- *)

  (* [discard] takes a token off the input stream, queries the lexer
     for a new one, and stores it into [env.token], overwriting the
     previous token. If [env.shifted] has not yet reached its limit,
     it is incremented. *)

  let discard env token : env' =
    Log.lookahead_token
      (fst3 token)
      (T.token2terminal (snd3 token))
      (thd3 token);
    let shifted = env.shifted + 1 in
    let shifted =
      if shifted >= 0
      then shifted
      else env.shifted
    in
    { env with token; shifted }

  (* --------------------------------------------------------------------------- *)

  (* In the code-based back-end, the [run] function is sometimes responsible
     for pushing a new cell on the stack. This is motivated by code sharing
     concerns. In this interpreter, there is no such concern; [run]'s caller
     is always responsible for updating the stack. *)

  (* In the code-based back-end, there is a [run] function for each state
     [s]. This function can behave in two slightly different ways, depending
     on when it is invoked, or (equivalently) depending on [s].

     If [run] is invoked after shifting a terminal symbol (or, equivalently,
     if [s] has a terminal incoming symbol), then [run] discards a token,
     unless [s] has a default reduction on [#]. (Indeed, in that case,
     requesting the next token might drive the lexer off the end of the input
     stream.)

     If, on the other hand, [run] is invoked after performing a goto transition,
     or invoked directly by an entry point, then there is nothing to discard.

     These two cases are reflected in [CodeBackend.gettoken].

     Here, the code is structured in a slightly different way. It is up to
     the caller of [run] to indicate whether to discard a token. *)

  let rec run env : parser =

    (* Log the fact that we just entered this state. *)

    let s = env.current in
    Log.state s;

    (* Examine what situation we are in. This case analysis is analogous to
       that performed in [CodeBackend.gettoken], in the sub-case where we do
       not have a terminal incoming symbol. *)

    T.default_reduction
      s
      reduce   (* there is a default reduction; perform it *)
      continue (* there is none; continue below *)
      env

  and continue env : parser =

    (* There is no default reduction. Consult the current lookahead token
       so as to determine which action should be taken. *)

    (* Peeking at the first input token, without taking it off the input
       stream, is normally done by reading [env.token]. However, we check
       [env.shifted] first: if it is -1, then the lookahead token is the
       [error] token. *)

    (* Note that, if we just called [discard] above, then the lookahead
       token cannot be [error]. *)

    if env.shifted = (-1) then begin
      Log.resuming_error_handling();
      Step (Step_error env)
    end
    else
      action env

  (* --------------------------------------------------------------------------- *)

  (* When [action] is invoked, we know that the current state does not have
     a default reduction. We also know that the current lookahead token is
     not [error]: it is a real token, stored in [env.token]. *)

  and action env : parser =

    (* We consult the two-dimensional action table, indexed by the
       current state and the current lookahead token, in order to
       determine which action should be taken. *)

    let token = snd3 env.token in
    T.action
      env.current                    (* determines a row *)
      (T.token2terminal token)       (* determines a column *)
      (T.token2value token)
      shift                          (* shift continuation *)
      reduce                         (* reduce continuation *)
      initiate                       (* failure continuation *)
      env

  (* --------------------------------------------------------------------------- *)

  (* This function takes care of shift transitions along a terminal symbol.
     (Goto transitions are taken care of within [reduce] below.) The symbol
     can be either an actual token or the [error] pseudo-token. *)

  and shift env
      (please_discard : bool)
      (terminal : terminal)
      (value : semantic_value)
      (s' : state)
      : parser =

    (* Log the transition. *)

    Log.shift terminal s';

    let env =
      let startp, _, endp = env.token in
      { env with
        (* Push a new cell onto the stack, containing the identity of the
           state that we are leaving. *)
        stack = {
          state = env.current;
          semv = value;
          startp;
          endp;
          next = env.stack;
        };
        (* Switch to state [s']. *)
        current = s';
      }
    in
    if please_discard
    then Feed (fun token -> Step_run (discard env token))
    else Step (Step_run env)

  (* --------------------------------------------------------------------------- *)

  (* This function takes care of reductions. *)

  and reduce env (prod : production) : parser =

    (* Log a reduction event. *)

    Log.reduce_or_accept prod;

    (* Invoke the semantic action. The semantic action is responsible for
       truncating the stack, updating the current state, producing a cell that
       contains a new semantic value, and raising [Accept] or [Error] if
       appropriate. *)

    (* If the semantic action raises [Error], we catch it immediately and
       initiate error handling. *)

    (* The apparently weird idiom used here is an encoding for a
       [let/unless] construct, which does not exist in ocaml. *)

    let env, success =
      try { env with stack = T.semantic_action prod env }, true
      with Error -> env, false
    in
    if success then begin

      (* By our convention, the semantic action is responsible for updating
         the stack. The state now found in the top stack cell is the return
         state. *)

      (* Perform a goto transition. The target state is determined
         by consulting the goto table at the return state and at
         production [prod]. *)

      let current = T.goto env.stack.state prod in
      Step (Step_run { env with current })

    end
    else
      errorbookkeeping env


  (* --------------------------------------------------------------------------- *)

  (* The following functions deal with errors. *)

  (* [initiate] and [errorbookkeeping] initiate error handling. See the functions
     by the same names in [CodeBackend]. *)

  and initiate env : parser =
    assert (env.shifted >= 0);
    if T.recovery && env.shifted = 0 then begin
      Log.discarding_last_token (T.token2terminal (snd3 env.token));
      Feed (fun token -> Step_action { (discard env token) with shifted = 0 })
    end
    else
      errorbookkeeping env

  and errorbookkeeping env =
    Log.initiating_error_handling();
    let env = { env with previouserror = env.shifted; shifted = -1 } in
    Step (Step_error env)

  (* [error] handles errors. *)

  and error env : parser =

    (* Consult the column associated with the [error] pseudo-token in the
       action table. *)

    T.action
      env.current                    (* determines a row *)
      T.error_terminal               (* determines a column *)
      T.error_value
      error_shift                    (* shift continuation *)
      error_reduce                   (* reduce continuation *)
      error_fail                     (* failure continuation *)
      env

  and error_shift env please_discard terminal value s' =

    (* Here, [terminal] is [T.error_terminal], and [value] is [T.error_value]. *)

    assert (terminal = T.error_terminal && value = T.error_value);

    (* This state is capable of shifting the [error] token. *)

    Log.handling_error env.current;
    shift env please_discard terminal value s'

  and error_reduce env prod =

    (* This state is capable of performing a reduction on [error]. *)

    Log.handling_error env.current;
    reduce env prod

  and error_fail env =

    (* This state is unable to handle errors. Attempt to pop a stack
       cell. *)

    let cell = env.stack in
    let next = cell.next in
    if next == cell then

      (* The stack is empty. Die. *)

      Reject

    else begin

      (* The stack is nonempty. Pop a cell, updating the current state
         with that found in the popped cell, and try again. *)

      let env = { env with stack = next; current = cell.state } in
      error env

    end

  (* --------------------------------------------------------------------------- *)

  let step = function
    | Step_run    env -> run env
    | Step_action env -> action env
    | Step_error  env -> error env

  (* --------------------------------------------------------------------------- *)

  let entry
      (s : state)
      (lexer : Lexing.lexbuf -> token)
      (lexbuf : Lexing.lexbuf)
      : semantic_value =

    (* Build an empty stack. This is a dummy cell, which is its own
       successor. Its fields other than [next] contain dummy values. *)

    let rec empty = {
      state = s;                          (* dummy *)
      semv = T.error_value;               (* dummy *)
      startp = lexbuf.Lexing.lex_start_p; (* dummy *)
      endp = lexbuf.Lexing.lex_curr_p;    (* dummy *)
      next = empty;
    } in

    (* Perform an initial call to the lexer. *)

    let token : token =
      lexer lexbuf
    in

    let { Lexing.lex_start_p; lex_curr_p } = lexbuf in

    (* Log our first lookahead token. *)

    Log.lookahead_token lex_start_p (T.token2terminal token) lex_curr_p;

    (* Build an initial environment. *)

    let env = {
      token = (lex_start_p, token, lex_curr_p);
      shifted = max_int;
      previouserror = max_int;
      stack = empty;
      current = s;
    } in

    (* Run. Catch [Accept], which represents normal termination. Let [Error]
       escape. *)

    try

      (* If ocaml offered a [match/with] construct with zero branches, this is
         what we would use here, since the type [void] has zero cases. *)

      let rec aux = function
        | Step s -> aux (step s)
        | Accept v -> v
        | Reject -> raise _eRR
        | Feed f ->
          let token = lexer lexbuf in
          let { Lexing.lex_start_p; lex_curr_p } = lexbuf in
          aux (step (f (lex_start_p, token, lex_curr_p)))
      in
      aux (run env)

    with
    | T.Accept v ->
        v

  (* --------------------------------------------------------------------------- *)

  (* Step-by-step execution interface *)

  let initial s =
    let rec empty = {
      state = s;                 (* dummy *)
      semv = T.error_value;      (* dummy *)
      startp = Lexing.dummy_pos; (* dummy *)
      endp = Lexing.dummy_pos;   (* dummy *)
      next = empty;
    } in

    let feed (start_p,t,curr_p as token) =
      (* Log our first lookahead token. *)

      Log.lookahead_token start_p (T.token2terminal t) curr_p;

      (* Build an initial environment. *)

      let env = {
        token = token;
        shifted = max_int;
        previouserror = max_int;
        stack = empty;
        current = s;
      } in

      Step_run env

    in
    Feed feed

  let step s =
    try step s
    with T.Accept v -> Accept v
       | Error      -> Reject

end

