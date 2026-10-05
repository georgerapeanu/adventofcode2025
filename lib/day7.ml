open! Core

let part1 () =
  let lines = In_channel.input_lines In_channel.stdin in
  let length = List.hd_exn lines |> String.length in
  List.fold lines
    ~init:(Array.init length ~f:(Fn.const 0))
    ~f:(fun acc row ->
      let chars = String.to_list row |> Array.of_list in
      let next_acc = Array.init (Array.length acc) ~f:(Fn.const 0) in
      Array.iteri chars ~f:(fun i c ->
          match c with
          | 'S' -> next_acc.(i) <- 1 + next_acc.(i)
          | '^' ->
              if acc.(i) > 0 then (
                next_acc.(i - 1) <- next_acc.(i - 1) + acc.(i);
                next_acc.(i + 1) <- next_acc.(i + 1) + 1)
          | '.' -> next_acc.(i) <- next_acc.(i) + acc.(i)
          | _ -> failwith "wrong");
      next_acc)
  |> Array.sum (module Int) ~f:Fn.id
  |> (fun x -> x - 1)
  |> Int.to_string |> print_endline

let part2 () =
  let lines = In_channel.input_lines In_channel.stdin in
  let length = List.hd_exn lines |> String.length in
  List.fold lines
    ~init:(Array.init length ~f:(Fn.const 0))
    ~f:(fun acc row ->
      let chars = String.to_list row |> Array.of_list in
      let next_acc = Array.init (Array.length acc) ~f:(Fn.const 0) in
      Array.iteri chars ~f:(fun i c ->
          match c with
          | 'S' -> next_acc.(i) <- 1 + next_acc.(i)
          | '^' ->
              next_acc.(i - 1) <- next_acc.(i - 1) + acc.(i);
              next_acc.(i + 1) <- next_acc.(i + 1) + acc.(i)
          | '.' -> next_acc.(i) <- next_acc.(i) + acc.(i)
          | _ -> failwith "wrong");
      next_acc)
  |> Array.sum (module Int) ~f:Fn.id
  |> Int.to_string |> print_endline
