let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

let directions =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let string_to_char_array s =
  let n = String.length s in
  Array.init n (fun i -> s.[i])

let in_bounds map r c =
  r >= 0 && r < Array.length map && c >= 0 && c < Array.length map.(r)

let is_full map r c = in_bounds map r c && map.(r).(c) = '@'

let count_neighbors map r c =
  List.fold_left
    (fun acc (dr, dc) ->
      let nr = r + dr in
      let nc = c + dc in
      if is_full map nr nc then acc + 1 else acc)
    0 directions

let rec dfs map stack count =
  match stack with
  | [] -> count
  | (r, c) :: rest ->
      let new_stack, newly_removed =
        List.fold_left
          (fun (stk_acc, cnt_acc) (dr, dc) ->
            let nr = r + dr in
            let nc = c + dc in
            if is_full map nr nc && count_neighbors map nr nc < 4 then (
              map.(nr).(nc) <- 'x';
              ((nr, nc) :: stk_acc, cnt_acc + 1))
            else (stk_acc, cnt_acc))
          (rest, 0) directions
      in
      dfs map new_stack (count + newly_removed)

let solve map =
  let initial_count, initial_stack =
    map
    |> Array.mapi (fun r row -> (r, row))
    |> Array.fold_left
         (fun (count, stack) (r, row) ->
           Array.mapi (fun c cell -> (c, cell)) row
           |> Array.fold_left
                (fun (count, stack) (c, cell) ->
                  if cell = '@' && count_neighbors map r c < 4 then (
                    map.(r).(c) <- 'x';
                    (count + 1, (r, c) :: stack))
                  else (count, stack))
                (count, stack))
         (0, [])
  in

  dfs map initial_stack initial_count

let run () =
  let map = read_input () |> List.map string_to_char_array |> Array.of_list in
  let result = solve map in
  Printf.printf "%d\n" result
