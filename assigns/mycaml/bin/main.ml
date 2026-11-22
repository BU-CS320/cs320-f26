let usage = "USAGE: dune exec mycaml <filename>"

let () =
  if Array.length Sys.argv <> 2
  then print_endline usage
  else
    let filename = Sys.argv.(1) in
    match Mycaml.interp ~filename with
    | Ok _ -> ()
    | Error e ->
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Mycaml.Error_msg.to_string ~filename ~text e in
           Format.eprintf "%s" msg)
