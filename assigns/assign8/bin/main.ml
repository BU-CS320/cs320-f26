let usage = "USAGE: dune exec assign8 <filename>"

let () =
  if Array.length Sys.argv <> 2
  then print_endline usage
  else
    let filename = Sys.argv.(1) in
    In_channel.with_open_text filename
      (fun ic ->
        ic
        |> In_channel.input_all
        |> Assign8.interp)
