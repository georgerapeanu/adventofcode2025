open! Core


let part1 () = 
  let ranges, points_with_space = 
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(String.strip)
    |> List.split_while ~f:(fun s -> not ( String.equal "" s) )
  in
  let points = List.tl_exn points_with_space |> List.map ~f:Int.of_string in
  let ranges = List.concat_map ranges ~f:(fun s ->
      match 
        String.split s ~on:'-' with
      | [x; y] -> [Int.of_string x, 1; Int.of_string y + 1, -1]
      | _ -> failwith "Wrong"
    )
  in 
  let deltas_array =
    List.sort ranges ~compare:(Comparable.lift Int.compare ~f:fst)
    |> Array.of_list
    |> Array.fold_map ~init:0 ~f:(fun sum (x, y) ->

        sum + y, (x, sum + y)
      ) |> snd
  in
  List.sum (module Int) points ~f:(fun point ->
      if (
        Array.binary_search deltas_array `Last_strictly_less_than point ~compare:(fun elem key ->
            Int.compare (fst elem) key
          )
        |> Option.map ~f:(fun i -> deltas_array.(i)) |> Option.value_map ~f:snd ~default:0) > 0 then 1 else 0
    )
  |> Int.to_string |> print_endline


let part2 () = 
  let ranges, _points_with_space = 
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(String.strip)
    |> List.split_while ~f:(fun s -> not ( String.equal "" s) )
  in
  let ranges = List.concat_map ranges ~f:(fun s ->
      match 
        String.split s ~on:'-' with
      | [x; y] -> [Int.of_string x, 1; Int.of_string y + 1, -1]
      | _ -> failwith "Wrong"
    )
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
  in 

  List.fold ranges ~init:(0, 0) ~f:(fun (ans, delta) (x, s)->
      ans + (if (delta = 0 && s > 0) then (-x) else if (delta > 0 && delta + s = 0) then (+x) else 0), delta + s
    )
  |> fst |> Int.to_string |> print_endline
