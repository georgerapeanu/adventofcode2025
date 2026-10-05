open Core

module Range = struct
  type t = {
    from: int
  ; to_: int
  }
  [@@deriving compare]

  let of_string s =
    match String.split s ~on:'-'  with
    | [a; b] -> {from = Int.of_string a; to_ = Int.of_string b}
    | _ -> failwith "Incorrect args"
end

let part1 () =
  let ranges = 
    In_channel.input_lines In_channel.stdin
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Range.of_string
    |> Array.of_list
  in
  Array.sort ranges ~compare:Range.compare;

  List.init 1000000 ~f:(fun x -> let s = Int.to_string x in
                         Int.of_string (s ^ s)
                       )
  |> List.sum (module Int) ~f:(fun x ->
      let range_x = {Range.from = x + 1; Range.to_ = x + 1} in
      let range = Array.binary_search ranges `Last_strictly_less_than range_x  ~compare:Range.compare in 
      match range with
      | None -> 0
      | Some i -> if ranges.(i).from <= x && x <= ranges.(i).to_ then x else 0
    )
  |> (fun x -> print_endline (Int.to_string x))


let part2 () =
  let ranges = 
    In_channel.input_lines In_channel.stdin
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Range.of_string
    |> Array.of_list
  in
  Array.sort ranges ~compare:Range.compare;

  let rec get_all_invalid_based_on_x acc last x =
    if (String.length last) + (String.length x) > 14 then
      acc
    else (
      let current = (last ^ x)  in
      get_all_invalid_based_on_x ((Int.of_string current)::acc) current x
    )
  in

  List.init 1000000 ~f:(fun x -> get_all_invalid_based_on_x [] (Int.to_string x) (Int.to_string x))
  |> List.concat
  |> List.dedup_and_sort ~compare:Int.compare
  |> List.sum (module Int) ~f:(fun x ->
      let range_x = {Range.from = x + 1; Range.to_ = x + 1} in
      let range = Array.binary_search ranges `Last_strictly_less_than range_x  ~compare:Range.compare in 
      match range with
      | None -> 0
      | Some i -> if ranges.(i).from <= x && x <= ranges.(i).to_ then x else 0
    )
  |> (fun x -> print_endline (Int.to_string x))
