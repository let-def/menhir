(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

include PreInterface

(* ------------------------------------------------------------------------ *)

(* Definitions to be exported for step-by-step engine interface. *)

let ty ?(p=[]) n = TypApp (n,p)

let tokenkind =
  if Settings.feed_nonterminal
  then ty "semantic_value"
  else TokenType.ttoken

let tytokentuple =
  TypTuple [
    ty "Lexing.position";
    tokenkind;
    ty "Lexing.position";
  ]

let steptypdefs =
  let tyenv = ty ~p:[ty "state"; ty "semantic_value"; tokenkind]
      "MenhirLib.EngineTypes.env"
  in
  [
    { typename = "state"; typeprivate = false;
      typerhs = TDefSum [];
      typeparams = []; typeconstraint = None };
    { typename = "feed"; typeprivate = false;
      typerhs = TAbbrev (ty "[ `Feed | `Feed_error ]");
      typeparams = []; typeconstraint = None };
    { typename = "step"; typeprivate = false;
      typerhs = TAbbrev (ty "[ `Step_run | `Step_error | `Step_action ]");
      typeparams = []; typeconstraint = None };
    { typename = "parser"; typeprivate = false;
      typerhs = TDefRecord [
          { modifiable = false;
            fieldname = "env";
            fieldtype = type2scheme tyenv;
          };
          { modifiable = false;
            fieldname = "tag";
            fieldtype = type2scheme (TypVar "a");
          };
        ];
      typeparams = ["a"]; typeconstraint = None };
  ]

let stepvaldecl =
  let result =
    "  [ `Step of step parser\n\
    \  | `Feed of feed parser\n\
    \  | `Accept of semantic_value\n\
    \  | `Reject ]"
  in
  [
    "initial", { quantifiers = [];
                 body = arrow (ty "state")
                          (arrow tytokentuple (ty ~p:[ty "step"] "parser")) };
    "step",    { quantifiers = [];
                 body = arrow (ty ~p:[ty "step"] "parser") (ty result) };
    "feed",    { quantifiers = [];
                 body = arrow (ty ~p:[ty "feed"] "parser")
                          (arrow tytokentuple (ty ~p:[ty "step"] "parser")) };
  ]

let querymoddef =
  let action_desc = "[`Shift | `Shift_and_discard | `Reduce | `Fail]" in
  "Query", {

    paramdecls = [];

    excdecls = [];

    typedecls = [
      { typename = "terminal"; typeprivate = true;
        typerhs = TAbbrev (ty "int");
        typeparams = []; typeconstraint = None }
    ];

    valdecls = [
      "index", type2scheme
        (arrow tokenkind (ty "terminal"));
      "action", type2scheme
        (arrow (ty "state") (arrow (ty "terminal") (ty action_desc)));
      "default_reduction", type2scheme
        (arrow (ty "state") (ty "bool"));
      "iter_states", type2scheme
        (arrow (arrow (ty "state") tunit) tunit);
    ];

    moddecls = [];

  }

let moddecls =
  if Settings.stepwise
  then [querymoddef]
  else []

let typedefs =
  PreInterface.interface.typedecls @
  if Settings.typed_values then
    let nonterminaltypedef =
      let add_nt sym ocamltype datadefs =
        {
          dataname = ntmangle sym;
          datavalparams = [TypTextual ocamltype];
          datatypeparams = None;
        } :: datadefs
      in
      let datadefs =
        StringMap.fold add_nt
          Front.grammar.UnparameterizedSyntax.types
          []
      in
      {
        typename = "nonterminal";
        typeparams = [];
        typerhs = TDefSum datadefs;
        typeconstraint = None;
        typeprivate = false;
      }
    in
    let valuetypedef =
      {
        typename = "semantic_value";
        typeparams = [];
        typerhs = TDefSum [
            {
              dataname = "Bottom";
              datavalparams = [];
              datatypeparams = None;
            };
            {
              dataname = "Terminal";
              datavalparams = [tokenkind];
              datatypeparams = None;
            };
            {
              dataname = "Nonterminal";
              datavalparams = [ty "nonterminal"];
              datatypeparams = None;
            }
          ];
        typeconstraint = None;
        typeprivate = false;
      }
    in
    [nonterminaltypedef; valuetypedef]
  else if Settings.stepwise then
    [ { typename = "semantic_value";
        typerhs = TAbbrev (ty "Obj.t");
        typeparams = []; typeconstraint = None; typeprivate = false } ]
  else
    []

let typedecls =
  typedefs @
  if Settings.stepwise
  then steptypdefs
  else []

let valdecls =
  PreInterface.interface.valdecls @
    if Settings.stepwise then
      let stepentryvaldecls =
        StringSet.fold (fun symbol decls ->
            (Misc.normalize symbol ^ "_state",
             {quantifiers = []; body = ty "state"}) :: decls
          ) PreFront.grammar.start_symbols []
      in
      stepvaldecl @ stepentryvaldecls
    else []

let interface =
  { PreInterface.interface with
    typedecls = typedecls;
    valdecls = valdecls;
    moddecls = moddecls;
  }

(* Writing the interface to a file. *)

let write () =
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface interface;
  close_out mli

