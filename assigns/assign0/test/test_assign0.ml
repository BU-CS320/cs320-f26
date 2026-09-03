open Assign0

let testing = true

let run cases b = if b then cases () else []

let sqrt_tests () =
  [
    assert (3 = sqrt 9);
    assert (4 = sqrt 10);
    assert (10 = sqrt 100);
    assert (11 = sqrt 120);
  ]


let is_prime_tests () =
  [
    assert (is_prime 2);
    assert (is_prime 37);
    assert (is_prime 97);
    assert (not (is_prime 0));
    assert (not (is_prime 1));
    assert (not (is_prime 57));
  ]

let _run_tests =
  if not testing then [] else
    [
      run sqrt_tests true;
      run is_prime_tests true;
    ]
