
let group (l : int list) : int list list option =
  let _ = l in
  assert false

type 'a rtree = Node of 'a * 'a rtree list

let split (t : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  let _ = t in
  assert false

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let _ = f, l in
  assert false

let apply_cycle (f : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let _ = f, n, x in
  assert false

let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let _ = f, n, ps in
  assert false
