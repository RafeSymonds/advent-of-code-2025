let read_input () =
  let rec loop acc =
    try loop (read_line () :: acc) with End_of_file -> List.rev acc
  in
  loop []

(* part 1 *)
(* let run () = *)
(*   let _, cnt = *)
(*     read_input () *)
(*     |> List.map (fun s -> *)
(*         (String.sub s 0 1, String.sub s 1 (String.length s - 1) |> int_of_string)) *)
(*     |> List.fold_left *)
(*          (fun (current, cnt) (dir, value) -> *)
(*            let current = current + if dir = "L" then -value else value in *)
(*            let current = current mod 100 in *)
(**)
(*            let cnt = if current = 0 then cnt + 1 else cnt in *)
(**)
(*            (current, cnt)) *)
(*          (50, 0) *)
(*   in *)
(*   Printf.printf "%d\n" cnt; *)
(*   () *)
(**)

(* part 2 *)
let run () =
  let _, cnt =
    read_input ()
    |> List.map (fun s ->
        (String.sub s 0 1, String.sub s 1 (String.length s - 1) |> int_of_string))
    |> List.fold_left
         (fun (current, cnt) (dir, value) ->
           let rec calc current cnt value =
             if value = 0 then (current, cnt)
             else
               let current = current + if dir = "L" then -1 else 1 in
               let current = current mod 100 in

               let cnt = if current = 0 then cnt + 1 else cnt in

               calc current cnt (value - 1)
           in

           calc current cnt value)
         (50, 0)
  in
  Printf.printf "%d\n" cnt;
  ()
