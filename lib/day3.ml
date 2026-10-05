open Core


(* I'm betting that part2 will be an extension where you just need to solve choosing k*)
let solve s k =
  let digits = 
    String.to_list s |> List.map ~f:(fun c -> String.of_char c |> Int.of_string)
  in


  let (final_stack, final_length), candidates = 
    List.fold_mapi digits ~init:(0, 0) ~f:(fun i (current_stack, length) digit ->
        let rec get_next_state (current_stack, length) =
          if length = 0 then (digit, 1)
          else if (current_stack % 10 < digit)  then get_next_state (current_stack / 10, length - 1)
          else if length < k then (current_stack * 10 + digit, length + 1)
          else (current_stack, length)
        in
        let candidate = if (List.length digits) - 1 - i >= k - length then
            let%map.Option () = Some () in
            Int.of_string ((Int.to_string current_stack) ^ (List.split_n digits ((List.length digits)  - (k - length)) |> snd |> List.map ~f:Int.to_string |> String.concat))
          else
            None
        in 
        (get_next_state (current_stack, length)), candidate
      ) 
  in

  let candidates = 
    let candidates = List.filter_opt candidates in
    if final_length >= k then final_stack :: candidates else candidates in 
  List.max_elt  candidates ~compare:Int.compare |> Option.value_exn

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.filter ~f:(fun s -> (String.strip s |> String.length) > 0)
  |> List.sum (module Int) ~f:(fun digits -> solve digits 2)
  |> (fun answer -> print_endline (Int.to_string answer))


let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.filter ~f:(fun s -> (String.strip s |> String.length) > 0)
  |> List.sum (module Int) ~f:(fun digits -> solve digits 12)
  |> (fun answer -> print_endline (Int.to_string answer))
