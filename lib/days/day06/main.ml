let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

(* let process operations rows = *)
(*   let sorted = List.init (List.length operations) (fun _ -> []) in *)
(**)
(*   let values = *)
(*     rows *)
(*     |> List.fold_left *)
(*          (fun output row -> *)
(*            List.map2 (fun out value -> value :: out) output row) *)
(*          sorted *)
(*   in *)
(**)
(*   List.map2 *)
(*     (fun op values -> *)
(*       match values with *)
(*       | [] -> failwith "empty values" *)
(*       | x :: xs -> List.fold_left op x xs) *)
(*     operations values *)
(**)
(* let run () = *)
(*   let input = *)
(*     read_input () *)
(*     |> List.map (fun s -> Str.split (Str.regexp "[ \t]+") s) *)
(*     |> List.rev *)
(*   in *)
(**)
(*   match input with *)
(*   | operations :: values -> *)
(*       let operations = *)
(*         operations *)
(*         |> List.map (function *)
(*           | "+" -> ( + ) *)
(*           | "*" -> ( * ) *)
(*           | _ -> failwith "invalid operation") *)
(*       in *)
(**)
(*       let values = *)
(*         values |> List.map (fun row -> row |> List.map int_of_string) *)
(*       in *)
(**)
(*       process operations values *)
(*       |> List.fold_left (fun acc value -> acc + value) 0 *)
(*       |> Printf.printf "%d\n" *)
(*   | _ -> failwith "bad" *)

(* part  *)

let is_only_whitespace s = String.trim s = ""

let transpose (lines : string list) : string list =
  let width = List.fold_left (fun m s -> max m (String.length s)) 0 lines in
  let padded =
    List.map
      (fun s ->
        if String.length s < width then
          s ^ String.make (width - String.length s) ' '
        else s)
      lines
  in

  let columns =
    List.init width (fun i -> List.map (fun row -> row.[i]) padded)
  in

  columns
  |> List.map (fun col -> col |> List.to_seq |> String.of_seq |> String.trim)
  |> List.filter (fun s -> s <> "")

let parse_blocks (lines : string list) : string list list =
  let rec aux current acc = function
    | [] ->
        if current = [] then List.rev acc else List.rev (List.rev current :: acc)
    | line :: rest ->
        let trimmed = String.trim line in
        let is_op =
          trimmed <> ""
          && (trimmed.[String.length trimmed - 1] = '+'
             || trimmed.[String.length trimmed - 1] = '*')
        in
        if is_op && current <> [] then
          aux [] (List.rev current :: acc) (line :: rest)
        else aux (line :: current) acc rest
  in
  aux [] [] lines

let calculate = function
  | first :: rest ->
      let re = Str.regexp "^\\([0-9]+\\)[ \t]*\\([+*]\\)$" in
      let first_value, op_str =
        if Str.string_match re first 0 then
          (Str.matched_group 1 first, Str.matched_group 2 first)
        else failwith ("Invalid first line: " ^ first)
      in

      let op =
        match op_str with
        | "+" -> ( + )
        | "*" -> ( * )
        | _ -> failwith "invalid operation"
      in

      rest |> List.map int_of_string
      |> List.fold_left op (int_of_string first_value)
  | _ -> failwith "invalid expression block"

let process lines =
  let columns = transpose lines in
  let blocks = parse_blocks columns in
  blocks |> List.map calculate |> List.fold_left ( + ) 0

let run () = read_input () |> process |> Printf.printf "%d\n"
