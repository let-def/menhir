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

end) = struct

  let order, reachability = 
    let result = Array.create G.n (-1) in
    let reachability = Array.create G.n CompressedBitSet.empty in
    let position = ref G.n in
    let stack = ref [] in
    (* Memoize function over DAG *)
    let memo f node =
      let index = G.index node in
      match result.(index) with
      (* Unmarked *)
      | -1 ->
        (* Temporary mark (node on stack) *)
        result.(index) <- -2;
	stack := node :: !stack;
        let r = f node in
        (* All successors of node marked, add the node itself to the
         * topological sort  *)
        decr position;
        result.(index) <- !position;
	stack := (match !stack with
	    | node' :: stack' when node == node' -> stack'
	    | _ -> assert false);
        reachability.(index) <- r;
        r
      (* Already on stack: not a DAG *)
      | -2 ->
	let rec aux acc = function
	  | node' :: _ when G.index node' = index -> node' :: acc
	  | node' :: tl -> aux (node' :: acc) tl
	  | _ -> assert false
	in
	raise (G.Cycle (aux [node] !stack))
      (* Already processed *)
      | _ -> reachability.(index)
    in
    (* Compute reachability set for each node *)
    let rec visit node =
      let aux succ acc =
        CompressedBitSet.union (memo visit succ) acc
      in
      G.successors aux node (CompressedBitSet.singleton (G.index node))
    in
    G.iter (fun node -> ignore (memo visit node));
    result, reachability

  let compare a b =
    let a, b = G.index a, G.index b in
    let oa, ob = order.(a), order.(b) in
    if oa = ob then
      Eq
    else if oa < ob && CompressedBitSet.mem b reachability.(a) then
      Lt
    else if oa > ob && CompressedBitSet.mem a reachability.(b) then
      Gt
    else
      Ic
end 

module Make (U : sig end) : sig

  type node
  exception Cycle of node list

  val fresh: string -> node
  val less_than: node -> node -> unit
  val check_for_cycle: unit -> node list option
  val compare: node -> node -> order
  val print: node -> string
  val successors: node -> node list

end = struct

  type node = {id: int; name: string; mutable succ: node list}
  exception Cycle of node list

  let n = ref 0
  let nodes = ref []

  let compare = ref (fun _ _ -> Ic)

  let update_compare n1 n2 =
    let module G =
      struct
	type t = node
	type node = t
	exception Cycle = Cycle
	let n = !n
	let index node = node.id
	let successors f node acc =
	  List.fold_left (fun acc node -> f node acc) acc node.succ
	let iter f = List.iter f !nodes
      end
    in
    let module R = Run (G) in
    compare := R.compare;
    R.compare n1 n2

  let fresh name =
    (*Printf.eprintf "fresh node %s\n%!" name;*)
    let node = {id = !n; name; succ = []} in
    nodes := node :: !nodes;
    compare := update_compare;
    incr n;
    node

  let less_than n1 n2 =
    (*Printf.eprintf "%s is less than %s\n%!" n1.name n2.name;*)
    n1.succ <- n2 :: n1.succ;
    compare := update_compare

  let compare a b = 
    let c = !compare a b in
    (*Printf.eprintf "%s %s %s\n%!"
      a.name
      (match c with Lt -> "<" | Eq -> "=" | Gt -> ">" | Ic -> "<>")
      b.name;*)
    c

  let check_for_cycle () =
    try match !nodes with
      | n1 :: _ -> ignore (compare n1 n1); None
      | [] -> None
    with Cycle n -> Some n

  let print {name} = name

  let successors n = n.succ

end
