let usage = "USAGE: dune exec assign9 <filename>"

let () =
  if Array.length Sys.argv <> 2
  then print_endline usage
  else
    let filename = Sys.argv.(1) in
    In_channel.with_open_text filename
      (fun ic ->
        let text = In_channel.input_all ic in
        Assign9.interp text)
