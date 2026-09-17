open Ws1

let _ = assert (digits_of_int 12345 = [1;2;3;4;5])
let _ = assert (digits_of_int (-12345) = [1;2;3;4;5])
let _ = assert (digits_of_int 0 = [0])
let _ = assert (digits_of_int_tr 12345 = [1;2;3;4;5])
let _ = assert (digits_of_int_tr (-12345) = [1;2;3;4;5])
let _ = assert (digits_of_int_tr 0 = [0])

let _ = assert (int_of_digits [1;2;3;4;5] = 12345)
let _ = assert (int_of_digits [0] = 0)
let _ = assert (int_of_digits [0;0;1;0;0] = 100)
let _ = assert (int_of_digits_tr [1;2;3;4;5] = 12345)
let _ = assert (int_of_digits_tr [0] = 0)
let _ = assert (int_of_digits_tr [0;0;1;0;0] = 100)

let _ = assert (gcd 9 15 = 3)

let _ = assert (py_triples 50 =
                [
                  (3, 4, 5);
                  (5, 12, 13);
                  (7, 24, 25);
                  (8, 15, 17);
                  (9, 40, 41);
                  (12, 35, 37);
                  (20, 21, 29)
                ])
