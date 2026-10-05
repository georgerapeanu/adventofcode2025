open! Core

module Junction = struct
  type t = { x : int; y : int; z : int } [@@deriving sexp]

  let of_string s =
    match String.split s ~on:',' |> List.map ~f:Int.of_string with
    | [ x; y; z ] -> { x; y; z }
    | _ -> failwith "too many per line"

  let sqr_distance t1 t2 =
    ((t1.x - t2.x) * (t1.x - t2.x))
    + ((t1.y - t2.y) * (t1.y - t2.y))
    + ((t1.z - t2.z) * (t1.z - t2.z))
end

module Dsu = struct
  module Inner = struct
    type t = { mutable father : int; mutable size : int } [@@deriving sexp]

    let create () : t = { father = -1; size = 1 }
  end

  type t = Inner.t Array.t

  let create n = Array.init n ~f:(fun _ -> Inner.create ())

  let rec get_root (t : t) x =
    if t.(x).father = -1 then x
    else
      let root = get_root t t.(x).father in
      t.(x).father <- root;
      root

  let union t x y =
    let x = get_root t x in
    let y = get_root t y in
    if x = y then false
    else
      let father, son = if t.(x).size > t.(y).size then (x, y) else (y, x) in
      t.(father).size <- t.(father).size + t.(son).size;
      t.(son).father <- father;
      true
end

let part1 () =
  let junctions =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:Junction.of_string
    |> Array.of_list
  in
  let dsu = Dsu.create (Array.length junctions) in
  let indexes = Array.init (Array.length junctions) ~f:Fn.id in
  Array.cartesian_product indexes indexes
  |> Array.filter_map ~f:(fun (i, j) ->
      if i >= j then None
      else Some (Junction.sqr_distance junctions.(i) junctions.(j), i, j))
  |> Array.sorted_copy ~compare:(Comparable.lift ~f:fst3 Int.compare)
  |> List.of_array
  |> (fun pairs -> List.take pairs 1000)
  |> List.iter ~f:(fun (_, i, j) -> Dsu.union dsu i j |> ignore);
  Array.filter_map dsu ~f:(fun { Dsu.Inner.father; size } ->
      if father = -1 then Some size else None)
  |> Array.sorted_copy ~compare:Int.compare
  |> Array.rev
  |> fun a -> print_endline (Int.to_string (a.(0) * a.(1) * a.(2)))

let part2 () =
  let junctions =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:Junction.of_string
    |> Array.of_list
  in
  let dsu = Dsu.create (Array.length junctions) in
  let indexes = Array.init (Array.length junctions) ~f:Fn.id in
  let last = ref 0 in
  Array.cartesian_product indexes indexes
  |> Array.filter_map ~f:(fun (i, j) ->
      if i >= j then None
      else Some (Junction.sqr_distance junctions.(i) junctions.(j), i, j))
  |> Array.sorted_copy ~compare:(Comparable.lift ~f:fst3 Int.compare)
  |> List.of_array
  |> List.iter ~f:(fun (_, i, j) ->
      if Dsu.union dsu i j then last := junctions.(i).x * junctions.(j).x);
  print_endline (Int.to_string !last)
