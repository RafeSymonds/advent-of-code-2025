let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

let num char = Char.code char - Char.code '0'

(* let process_line line = *)
(*   let rec process_digit index best current = *)
(*     if index = String.length line then best *)
(*     else *)
(*       let value = num line.[index] in *)
(**)
(*       let new_value = (current * 10) + value in *)
(**)
(*       let best = if new_value > best then new_value else best in *)
(*       let current = Stdlib.max current value in *)
(**)
(*       process_digit (index + 1) best current *)
(*   in *)
(**)
(*   process_digit 0 0 0 *)
(**)

let process_line line =
  let rec process_digit index best current s =
    if index = String.length line then best
    else
      let value = num line.[index] in

      let best = Stdlib.max best current in
      let num_left = String.length line - index - 1 in

      let rec pop current =
        if Stack.length s = 0 then (
          Stack.push value s;
          (current * 10) + value)
        else if Stack.length s + num_left >= 12 && Stack.top s < value then
          let _ = Stack.pop s in
          pop (current / 10)
        else if Stack.length s < 12 then (
          Stack.push value s;
          (current * 10) + value)
        else current
      in

      let current = pop current in
      process_digit (index + 1) (Stdlib.max best current) current s
  in
  process_digit 0 0 0 (Stack.create ())

let run () =
  read_input () |> List.map process_line
  |> List.fold_left (fun acc x -> acc + x) 0
  |> Printf.printf "%d\n"
