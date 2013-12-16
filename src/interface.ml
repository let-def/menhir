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

let tokenkind =
  if Settings.feed_nonterminal
  then "semantic_value"
  else "token"

let ty n = TypApp (n,[])

let steptypdefs =
  let tyenv = TypApp ("MenhirLib.EngineTypes.env",
                      [ty "state"; ty "semantic_value"; ty tokenkind])
  in
  [
    { typename = "state"; typerhs = TAbbrev (ty "int");
      typeparams = []; typeconstraint = None; typeprivate = true };
    { typename = "step"; typeprivate = true;
      typerhs = TDefSum [
          { dataname = "Step_run";
            datavalparams = [tyenv];
            datatypeparams = None
          };
          { dataname = "Step_error";
            datavalparams = [tyenv];
            datatypeparams = None
          };
          { dataname = "Step_action";
            datavalparams = [tyenv];
            datatypeparams = None
          };
        ];
      typeparams = []; typeconstraint = None };
    { typename = "parser"; typeprivate = false;
      typerhs = TDefSum [
          { dataname = "Step";
            datavalparams = [ty "step"];
            datatypeparams = None
          };
          { dataname = "Accept";
            datavalparams = [ty "semantic_value"];
            datatypeparams = None
          };
          { dataname = "Reject";
            datavalparams = [];
            datatypeparams = None
          };
          { dataname = "Feed";
            datavalparams = [TypArrow (TypTuple [
                ty "Lexing.position";
                ty tokenkind;
                ty "Lexing.position";
              ], ty "step")];
            datatypeparams = None
          };
        ];
      typeparams = []; typeconstraint = None };
  ]

let stepvaldecl =
  [
    "initial", { quantifiers = []; body = arrow (ty "state") (ty "parser") };
    "step",    { quantifiers = []; body = arrow (ty "step") (ty "parser") };
  ]

let querymoddef =
  let action_desc = "[`Shift | `Shift_and_discard | `Reduce | `Fail]" in
  "Query", {

    paramdecls = [];

    excdecls = [];

    typedecls = [
      { typename = "terminal"; typerhs = TDefSum [];
        typeparams = []; typeconstraint = None; typeprivate = false };
    ];

    valdecls = [
      "index", type2scheme
        (arrow (ty tokenkind) (ty "terminal"));
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
              datavalparams = [TypTextual (Stretch.Inferred "token")];
              datatypeparams = None;
            };
            {
              dataname = "Nonterminal";
              datavalparams = [TypTextual (Stretch.Inferred "nonterminal")];
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

