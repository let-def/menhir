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

(* This module instantiates the generic [Engine] with a thin decoding layer
   for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

(* The exception [Accept] is no longer pre-declared here: with --typed-values,
   the Table backend now generates an exception parametrized over the type of
   semantic values. *)

(* This functor is invoked by the generated parser. *)

module Make (T : TableFormat.TABLES)

: EngineTypes.ENGINE with type state = int
		      and type token = T.token
		      and type semantic_value = T.semantic_value

