open! Core

let part1 () =
  let all_input = In_channel.input_lines In_channel.stdin in
  let rows =
    List.rev all_input |> List.tl_exn |> List.rev |> Array.of_list
    |> Array.map ~f:(fun line ->
        String.split line ~on:' '
        |> List.filter ~f:(fun s -> not (String.equal (String.strip s) ""))
        |> List.map ~f:Int.of_string |> Array.of_list)
  in
  let signs =
    List.rev all_input |> List.hd_exn |> String.split ~on:' '
    |> List.filter ~f:(fun s -> not (String.equal (String.strip s) ""))
  in
  List.mapi signs ~f:(fun i sign ->
      Array.map rows ~f:(fun row -> row.(i))
      |> Array.fold
           ~init:(if String.equal sign "*" then 1 else 0)
           ~f:(fun acc v -> if String.equal sign "*" then acc * v else acc + v))
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string |> print_endline

let part2 () =
  let all_input = In_channel.input_lines In_channel.stdin in
  let signs =
    List.rev all_input |> List.hd_exn |> String.to_list
    |> List.filter_mapi ~f:(fun i s ->
        if Char.equal s ' ' then None else Some (i, s))
    |> Array.of_list
  in
  let rows =
    List.rev all_input |> List.tl_exn |> List.rev
    |> List.map ~f:(fun s -> String.to_list s |> Array.of_list)
    |> Array.of_list
  in
  Array.mapi signs ~f:(fun i (start_index, s) ->
      let end_index =
        if i + 1 < Array.length signs then (signs.(i + 1) |> fst) - 1
        else
          Array.map rows ~f:(fun row -> Array.length row)
          |> Array.max_elt ~compare:Int.compare
          |> Option.value_exn
      in
      let len = end_index - start_index in
      let numbers =
        Array.map rows ~f:(fun row -> Array.sub row ~pos:start_index ~len)
      in
      List.init len ~f:(fun i ->
          Array.fold numbers ~init:0 ~f:(fun acc number ->
              match Int.of_string_opt (Char.to_string number.(i)) with
              | Some d -> (acc * 10) + d
              | None -> acc))
      |> List.fold
           ~init:(if Char.equal s '*' then 1 else 0)
           ~f:(fun acc v -> if Char.equal s '*' then acc * v else acc + v))
  |> Array.sum (module Int) ~f:Fn.id
  |> Int.to_string |> print_endline
