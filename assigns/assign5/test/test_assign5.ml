open Assign5

let testing = true

let run cases b = if b then cases () else []

let group_tests () =
  [
    assert (group [1;2;3;0;-1;-2;-3;0;1] = Some [[1;2;3];[-1;-2;-3];[1]]);
    assert (group [1;2;3;0;1;2;3;0;1] = None);
    assert (group [0;1;2;3] = None);
    assert (group [1;0;0;-1] = None);
    (* add additional tests here *)
  ]

let split_tests () =
  [
    (
      let t = Node ((1, "a"), [Node ((2, "b"), []); Node ((3, "c"), [])]) in
      assert (split t
              = ( Node (1, [Node (2, []); Node (3, [])])
                , Node ("a", [Node ("b", []); Node ("c", [])])))
    );
    assert (split (Node ((1, "a"), [])) = (Node (1, []), Node ("a", [])));
    (* add additional tests here *)
  ]

let prefix_map_tests () =
  let f pre = let s = List.fold_left (+) 0 pre in if s >= 5 then Some s else None in
  [
    assert (prefix_map f [1;2;3;4] = Some (6, [4]));
    assert (prefix_map f [1;1] = None);
    (* add additional tests here *)
  ]

let apply_cycle_tests () =
  let f x = x + 1 in
  let g x = x - 1 in
  let h x = x * x in
  let k x = x / 2 in
  [
    assert (apply_cycle [f;g;g] 8 0 = -2);
    assert (apply_cycle [g;f;f] 8 0 = 2);
    assert (apply_cycle [f;g;g] 0 10 = 10);
    assert (apply_cycle [f;h;k] 4 5 = 19);
    (* add additional tests here *)
  ]

let walks_tests () =
  let g1 (i : int) (j : int) = i < j && i <= 10 && j <= 10 in
  let g2 (i : int) (j : int) = i <= 10 && j <= 10 in
  let p1 i = i + 1 in
  let p2 i = i - 1 in
  let p3 i = i + 2 in
  [
    assert (walks g1 0 [(p1, 0); (p2, 0); (p3, 0)] = [0;0;0]);
    assert (walks g1 1 [(p1, 0); (p2, 0); (p3, 0)] = [1;2]);
    assert (walks g1 3 [(p1, 0); (p2, 0); (p3, 0)] = [3;6]);
    assert (walks g1 6 [(p1, 0); (p2, 0); (p3, 0)] = [6]);
    assert (walks g2 2 [(p1, 3); (p2, 5); (p3, 3)] = [5; 3; 7]);
    assert (walks g2 4 [(p1, -10); (p2, -20); (p3, 8)] = [-6; -24]);
    assert (walks g2 6 [(p1, 5); (p2, 11); (p3, -10)] = [2]);
    (* add additional tests here *)
  ]

let _run_tests =
  if not testing then [] else
    [
      run group_tests true;
      run split_tests true;
      run prefix_map_tests true;
      run apply_cycle_tests true;
      run walks_tests true;
    ]
