open! Core

let part1 () =
  let matrix = 
    In_channel.input_lines In_channel.stdin
    |> Array.of_list
    |> Array.map ~f:(fun s ->
        String.to_list s |> Array.of_list
      )
  in
  let n = Array.length matrix in let m = Array.length matrix.(0) in
  List.cartesian_product
    (List.init n ~f:Fn.id)
    (List.init m ~f:Fn.id)
  |> List.sum (module Int) ~f:(fun (x, y) -> (
        let surround = 
          List.cartesian_product
            (List.init 3 ~f:(fun x -> x - 1))
            (List.init 3 ~f:(fun x -> x - 1))
          |> List.sum (module Int) ~f:(fun (dx, dy) ->
              if x + dx >=0 && x + dx < n && y + dy >= 0 && y + dy < m && Char.equal matrix.(x + dx).(y + dy) '@' then 1 else 0
            )
        in
        if Char.equal matrix.(x).(y) '@' && surround < 5 then 1 else 0
      ))

  |> (fun s -> print_endline (Int.to_string s))



let part2 () =
  let matrix = 
    In_channel.input_lines In_channel.stdin
    |> Array.of_list
    |> Array.map ~f:(fun s ->
        String.to_list s |> Array.of_list
      )
  in
  let n = Array.length matrix in let m = Array.length matrix.(0) in
  let eval x y =

    List.cartesian_product
      (List.init 3 ~f:(fun x -> x - 1))
      (List.init 3 ~f:(fun x -> x - 1))
    |> List.sum (module Int) ~f:(fun (dx, dy) ->
        if x + dx >=0 && x + dx < n && y + dy >= 0 && y + dy < m && Char.equal matrix.(x + dx).(y + dy) '@' then 1 else 0
      )
  in 
  let q = Queue.create () in 
  let _ = 
    Array.init n ~f:(fun i -> Array.init m ~f:(fun j ->
        if Char.equal matrix.(i).(j) '.' then Queue.enqueue q (i, j);
        0
      )) in
  let rec process acc =
    match Queue.dequeue q with
    | None -> acc
    | Some (x, y) ->
      process 
      (List.cartesian_product
        (List.init 3 ~f:(fun x -> x - 1))
        (List.init 3 ~f:(fun x -> x - 1))
      |> List.fold ~init:acc ~f:(fun acc (dx, dy) ->
          if x + dx >=0 && x + dx < n && y + dy >= 0 && y + dy < m && Char.equal matrix.(x + dx).(y + dy) '@' && eval (x + dx) (y + dy) < 5 then (
            matrix.(x + dx).(y + dy) <- '.';
            Queue.enqueue q (x + dx, y + dy);
            acc + 1
          ) else acc
        ))
  in
  process 0 |> Int.to_string |> print_endline


