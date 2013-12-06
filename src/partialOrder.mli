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

end) : sig

  open G

  val compare: node -> node -> order
end

module Make (U : sig end) : sig

  type node

  val fresh: string -> node
  val less_than: node -> node -> unit

  val compare: node -> node -> order

  val print: node -> string
end
