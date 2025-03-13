
(*
  imperative file reading solution, excluding new_lines
  found from stack overflow by user gascha in this answer:
  https://stackoverflow.com/a/15848796
*)
let read_file file =
  let ic = open_in file in
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done; assert false
  with End_of_file ->
  String.concat "\n" (List.rev !lines)
