(* part 1*)

(* let invalid num = *)
(*   let s = string_of_int num in *)
(*   let len = String.length s in *)
(*   if len mod 2 <> 0 then false *)
(*   else *)
(*     let half = len / 2 in *)
(*     String.sub s 0 half = String.sub s half half *)

(* part 2*)

let invalid num =
  let s = string_of_int num in
  let len = String.length s in

  let rec invalid_sub_length l =
    if l >= len then false
    else if len mod l <> 0 then invalid_sub_length (l + 1)
    else
      let first = String.sub s 0 l in

      let rec iterate_substrings offset =
        if offset = len then true
        else
          let sub = String.sub s offset l in
          if first <> sub then false else iterate_substrings (offset + l)
      in

      if iterate_substrings l then true else invalid_sub_length (l + 1)
  in
  invalid_sub_length 1

let count_invalid_range input =
  if input = "" then 0
  else
    match String.split_on_char '-' input with
    | [ start_val; end_val ] ->
        let start_val = int_of_string start_val in
        let end_val = int_of_string end_val in
        List.init (end_val - start_val + 1) (fun i -> start_val + i)
        |> List.fold_left (fun acc v -> if invalid v then acc + v else acc) 0
    | _ -> failwith "expected range"

let run () =
  let x =
    read_line () |> String.split_on_char ','
    |> List.fold_left (fun acc input -> acc + count_invalid_range input) 0
  in
  Printf.printf "%d\n" x
