open Practice

let _ = assert (negatives [1;2;3;-2;-3;1] = [-2;-3])

let _ = assert (gets 1 [(1, "1"); (2, "2"); (1, "3")] = ["1"; "3"])
