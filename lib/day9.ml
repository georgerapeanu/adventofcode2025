open! Core

module Tile = struct
  type t = { x : int; y : int } [@@deriving compare, fields ~getters, sexp]

  let of_string s =
    match String.split s ~on:',' |> List.map ~f:Int.of_string with
    | [ x; y ] -> { x; y }
    | s -> raise_s [%message "Wrong number of things" (s : int list)]
end

let part1 () =
  let tiles =
    In_channel.input_lines In_channel.stdin |> List.map ~f:Tile.of_string
  in
  List.cartesian_product tiles tiles
  |> List.map ~f:(fun (t1, t2) ->
      (abs (t1.x - t2.x) + 1) * (abs (t1.y - t2.y) + 1))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> fun x -> print_endline (Int.to_string x)

let part2 () =
  let tiles =
    In_channel.input_lines In_channel.stdin |> List.map ~f:Tile.of_string
  in
  let vertical_splitters = Int.Table.create () in
  let horizontal_splitters = Int.Table.create () in
  List.zip_exn (List.tl_exn tiles @ [ List.hd_exn tiles ]) tiles
  |> List.iter ~f:(fun (fst, snd) ->
      if fst.x = snd.x then
        let sgn = if snd.y > fst.y then 1 else -1 in
        List.init ((snd.y - fst.y) * sgn) ~f:(fun y -> fst.y + (y * sgn))
        |> List.iter ~f:(fun y ->
            Hashtbl.update vertical_splitters y ~f:(function
              | None -> [ fst.x ]
              | Some existing -> fst.x :: existing))
      else
        let sgn = if snd.x > fst.x then 1 else -1 in
        List.init ((snd.x - fst.x) * sgn) ~f:(fun x -> fst.x + (x * sgn))
        |> List.iter ~f:(fun x ->
            Hashtbl.update horizontal_splitters x ~f:(function
              | None -> [ fst.y ]
              | Some existing -> fst.y :: existing)));
  let vertical_splitters =
    Hashtbl.map vertical_splitters ~f:(fun splitters ->
        List.sort ~compare:Int.compare splitters |> Array.of_list)
  in
  let horizontal_splitters =
    Hashtbl.map horizontal_splitters ~f:(fun splitters ->
        List.sort ~compare:Int.compare splitters |> Array.of_list)
  in
  List.cartesian_product tiles tiles
  |> List.filter_map ~f:(fun (t1, t2) ->
      let min_x = min t1.x t2.x in
      let min_y = min t1.y t2.y in
      let max_x = max t1.x t2.x in
      let max_y = max t1.y t2.y in
      let area = (max_x - min_x + 1) * (max_y - min_y + 1) in

      let valid =
        List.for_all
          [
            (horizontal_splitters, min_x, min_y, max_y);
            (horizontal_splitters, max_x, min_y, max_y);
            (vertical_splitters, min_y, min_x, max_x);
            (vertical_splitters, max_y, min_x, max_x);
          ]
          ~f:(fun (splitters, key, from, to_) ->
            match Hashtbl.find splitters key with
            | None -> true
            | Some splitters -> (
                match
                  Array.binary_search splitters ~compare:Int.compare
                    `First_strictly_greater_than from
                with
                | Some pos when splitters.(pos) < to_ -> false
                | Some _ | None -> true))
      in
      if valid then Some area else None)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> fun x -> print_endline (Int.to_string x)
