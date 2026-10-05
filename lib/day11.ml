open! Core

module Graph = struct
  module Edge = struct
    module T = struct
      type t = { from : string; to_ : string }
      [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  module Edge_for_part_two = struct
    module T = struct
      type t = { from : string; to_ : string; state : int }
      [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  let t_parser =
    let open Angstrom in
    lift2
      (fun from to_ -> (from, to_))
      (take_while1 (function ':' -> false | _ -> true) <* char ':')
      (many
         (take_till (function ' ' -> false | _ -> true)
         *> take_while1 (function ' ' -> false | _ -> true)))

  let of_input lines =
    List.map lines ~f:(fun line ->
        Angstrom.parse_string ~consume:All t_parser line
        |> Result.ok_or_failwith)
    |> String.Map.of_alist_exn

  let count_paths t from to_ =
    let count_paths =
      Memo.recursive ~hashable:Edge.hashable (fun self { Edge.from; to_ } ->
          if String.equal from to_ then 1
          else
            List.sum
              (module Int)
              (Map.find t from |> Option.value ~default:[])
              ~f:(fun neighbor -> self { Edge.from = neighbor; to_ }))
    in
    count_paths { from; to_ }

  let count_paths_part2 t from to_ =
    let count_paths =
      Memo.recursive ~hashable:Edge_for_part_two.hashable
        (fun self { from; to_; state } ->
          let state =
            match from with
            | "dac" -> state lor 1
            | "fft" -> state lor 2
            | _ -> state
          in
          if String.equal from to_ then if state = 3 then 1 else 0
          else
            List.sum
              (module Int)
              (Map.find t from |> Option.value ~default:[])
              ~f:(fun neighbor -> self { from = neighbor; to_; state }))
    in
    count_paths { from; to_; state = 0 }
end

let part1 () =
  let graph = In_channel.input_lines In_channel.stdin |> Graph.of_input in
  Graph.count_paths graph "you" "out" |> fun x ->
  print_endline (Int.to_string x)

let part2 () =
  let graph = In_channel.input_lines In_channel.stdin |> Graph.of_input in
  Graph.count_paths_part2 graph "svr" "out" |> fun x ->
  print_endline (Int.to_string x)
