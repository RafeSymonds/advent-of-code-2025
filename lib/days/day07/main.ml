let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

let process_beams prev_row splitters =
  let w = List.length prev_row in
  let next_row = Array.make w 0 in

  List.iteri
    (fun i active ->
      if active > 0 then
        if List.nth splitters i then (
          next_row.(i - 1) <- next_row.(i - 1) + active;
          next_row.(i + 1) <- next_row.(i + 1) + active)
        else next_row.(i) <- next_row.(i) + active)
    prev_row;

  Array.to_list next_row

let run () =
  let input = read_input () in
  match input with
  | start_line :: rest ->
      let start_col = String.index start_line 'S' in
      let width = String.length start_line in

      let prev_row =
        List.init width (fun i -> if i = start_col then 1 else 0)
      in

      rest
      |> List.map (fun s ->
          s |> String.to_seq |> Seq.map (fun c -> c = '^') |> List.of_seq)
      |> List.fold_left (fun prev row -> process_beams prev row) prev_row
      |> List.fold_left (fun total x -> total + x) 0
      |> Printf.printf "\n%d\n"
  | _ -> failwith "invalid input"
