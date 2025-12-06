let read_input () =
  let rec loop acc =
    match read_line () with
    | "" -> List.rev acc
    | s -> loop (s :: acc)
    | exception End_of_file -> List.rev acc
  in
  let ranges = loop [] in
  let ids =
    let rec loop2 acc =
      match read_line () with
      | s -> loop2 (s :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop2 []
  in
  (ranges, ids)

let is_fresh ranges id =
  ranges
  |> List.exists (fun (start_id, end_id) -> start_id <= id && id <= end_id)

let merge intervals =
  let rec merge result = function
    | [] -> result |> List.rev
    | [ interval ] -> interval :: result |> List.rev
    | (s1, e1) :: (s2, e2) :: rest ->
        if e1 < s2 then merge ((s1, e1) :: result) ((s2, e2) :: rest)
        else merge result ((s1, Stdlib.max e1 e2) :: rest)
  in
  merge [] intervals

let run () =
  let ranges, _ = read_input () in

  let ranges =
    ranges
    |> List.map (fun s -> String.split_on_char '-' s)
    |> List.map (function
      | [ start_id; end_id ] -> (int_of_string start_id, int_of_string end_id)
      | _ -> failwith "invalid input")
  in

  (* part 1 *)
  (* ids |> List.map int_of_string *)
  (* |> List.fold_left *)
  (*      (fun total id -> if is_fresh ranges id then total + 1 else total) *)
  (*      0 *)
  (* |> Printf.printf "%d\n" *)

  (* part 2 *)
  ranges
  |> List.sort (fun (s1, e1) (s2, e2) ->
      if s1 = s2 then compare e1 e2 else compare s1 s2)
  |> merge
  |> List.fold_left (fun total (s, e) -> total + e - s + 1) 0
  |> Printf.printf "%d\n"
