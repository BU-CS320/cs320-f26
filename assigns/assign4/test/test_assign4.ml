open Assign4

let testing = true

let run cases b = if b then cases () else []

let split_tests () =
  [
    assert (split_by_ws "1 23 + -94  *" = ["1"; "23"; "+"; "-94"; "*"]);
    assert (split_by_ws "  foo    bar  \n\n   baz   " = ["foo"; "bar"; "baz"]);
    (* add more tests here *)
  ]

let is_close f1 f2 = Float.abs (f1 -.f2) < 10. ** (-15.)
let dist_tests () =
  [
    assert (is_close (dist [N; N; S; E]) (sqrt 2.));
    (* add more tests here *)
  ]

let convert_tests () =
  [
    assert
      (convert [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
       = [Int_list [2;3]; String_list ["a";"b"]; Int_list [4]; String_list ["c"]]);
    (* add more tests here *)
  ]

let t = Node (5, Empty, Empty)
let insert_tests () =
  [
    assert (insert 5 Empty = Node (5, Empty, Empty));

    assert (insert 3 t = Node (5, Node (3, Empty, Empty), Empty));
    assert (insert 8 t = Node (5, Empty, Node (8, Empty, Empty)));
    assert
      (insert 4 (insert 8 (insert 3 (insert 5 Empty)))
       = Node (5, Node (3, Empty, Node (4, Empty, Empty)), Node (8, Empty, Empty)));
    (* add more tests here *)
  ]

let flatten_tests () =
  [
    assert (flatten (insert 4 (insert 8 (insert 3 (insert 5 Empty)))) = [3; 4; 5; 8]);
    (* add more tests here *)
  ]

let sort_tests () =
  [
    assert (sort [5; 3; 8; 1; 4] = [1; 3; 4; 5; 8]);
  ]

let _run_tests =
  if not testing then [] else
    [
      run split_tests true;
      run dist_tests true;
      run convert_tests true;
    ]
