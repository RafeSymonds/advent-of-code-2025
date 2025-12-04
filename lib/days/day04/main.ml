let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

let run () =
  let _input = read_input () in
  Printf.printf "TODO: implement Day04\n"
