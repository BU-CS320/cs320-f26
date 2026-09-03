open Assign1

let testing = true

let run cases b = if b then cases () else []

let num_digits_tests () =
  [
    assert (num_digits 0 = 1);
    assert (num_digits 7 = 1);
    assert (num_digits 12345 = 5);
    assert (num_digits (-121) = 3);
  ]

let is_perfect_pow_tests () =
  [
    assert (is_perfect_pow 1 16);
    assert (is_perfect_pow 2 16);
    assert (not (is_perfect_pow 3 16));
    assert (is_perfect_pow 4 16);
    assert (is_perfect_pow 3 (-8));
  ]

let num_factors_tests () =
  [
    assert (num_factors 16 = 4);
    assert (num_factors 18 = 3);
    assert (num_factors 19 = 1);
  ]

let is_hypotenuse_tests () =
  [
    assert (is_hypotenuse 5);
    assert (is_hypotenuse 13);
    assert (is_hypotenuse 17);
    assert (is_hypotenuse 29);
    assert (not (is_hypotenuse 28));
    assert (not (is_hypotenuse 6));
  ]

let drop_leading_tests () =
  [
    assert (drop_leading 1 [1;1;2;1;3] = [2;1;3]);
    assert (drop_leading 5 [1;2;3] = [1;2;3]);
    assert (drop_leading 2 [2;2;2] = []);
  ]

let _run_tests =
  if not testing then [] else
    [
      run num_digits_tests true;
      run is_perfect_pow_tests true;
      run num_factors_tests true;
      run is_hypotenuse_tests true;
      run drop_leading_tests true;
    ]
