type order = Lt | Gt | Eq | Ic

module Run (G : sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Fold over a node's immediate successors. *)

  val successors: (node -> 'a -> 'a) -> node -> 'a -> 'a

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

  (* Exception raised during functor instanciation if a cycle is detected *)

  exception Cycle of node list

end) : sig

  open G
  val compare: node -> node -> order

end


module Make (U : sig end) : sig

  type node
  exception Cycle of node list

  (* Create a new named node *)
  val fresh: string -> node

  (* [less_than a b] makes [a] less than [b] in the partial order computed by
     [compare]. *)
  val less_than: node -> node -> unit

  (* Check if the partial order defined with calls to [less_than] contains a
     cycle. *)
  val check_for_cycle: unit -> node list option

  (* Compare two nodes, or raise [Cycle nodes] if there is a cycle. *)
  val compare: node -> node -> order

  (* Get back the name of a node. *)
  val print: node -> string

  (* List immediate successors in the graph defined by the partial ordering. *)
  val successors: node -> node list

end
