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

let steptypdefs =
  let tokenkind =
    if Settings.feed_nonterminal
    then "semantic_value"
    else "token"
  in
  let tyenv = TypApp ("MenhirLib.EngineTypes.env", [
      TypApp ("state",[]); TypApp ("semantic_value",[]); TypApp (tokenkind,[]);
    ])
  in
  [
    { typename = "state"; typerhs = TDefSum [];
      typeparams = []; typeconstraint = None; typeprivate = false };
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
    { typename = "outcome"; typeprivate = false;
      typerhs = TDefSum [
          { dataname = "Step";
            datavalparams = [TypApp ("step",[])];
            datatypeparams = None
          };
          { dataname = "Accept";
            datavalparams = [TypApp ("semantic_value",[])];
            datatypeparams = None
          };
          { dataname = "Reject";
            datavalparams = [];
            datatypeparams = None
          };
          { dataname = "Feed";
            datavalparams = [TypArrow (TypTuple [
                TypApp ("Lexing.position",[]);
                TypApp (tokenkind,[]);
                TypApp ("Lexing.position",[]);
              ], TypApp ("step",[]))];
            datatypeparams = None
          };
        ];
      typeparams = []; typeconstraint = None };
  ]

let stepvaldecl =
  let ty n = TypApp (n,[]) in
  [
    "initial", { quantifiers = []; body = arrow (ty "state") (ty "outcome") };
    "step",    { quantifiers = []; body = arrow (ty "step") (ty "outcome") };
  ]

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
        typerhs = TAbbrev (TypApp ("Obj.t", []));
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
             {quantifiers = []; body = TypApp ("state",[])}) :: decls
          ) PreFront.grammar.start_symbols []
      in
      stepvaldecl @ stepentryvaldecls
    else []

let interface =
  { PreInterface.interface with
    typedecls = typedecls;
    valdecls = valdecls;
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

