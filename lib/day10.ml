open! Core

module Machine = struct
  module Joltage = struct
    module T = struct
      type t = int list [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end

  type t = {
    target_lights : int;
    buttons : int list;
    target_joltage : Joltage.t;
  }
  [@@deriving compare, fields ~getters, sexp]

  let parser =
    let open Angstrom in
    let target =
      char '['
      *> ( take_while1 (function '#' | '.' -> true | _ -> false) >>| fun s ->
           String.rev s
           |> String.fold ~init:0 ~f:(fun acc -> function
             | '#' -> (2 * acc) + 1
             | '.' -> 2 * acc
             | _ -> failwith "wtf") )
      <* char ']' <* char ' '
    in
    let buttons =
      many
        (char '('
         *> lift2
              (fun lights light ->
                List.fold (light :: lights) ~init:0 ~f:(fun acc light ->
                    acc lxor (1 lsl light)))
              (many
                 (take_while1 (function '0' .. '9' -> true | _ -> false)
                 >>| Int.of_string <* char ','))
              (take_while1 (function '0' .. '9' -> true | _ -> false)
              >>| Int.of_string)
        <* char ')' <* char ' ')
    in
    let joltage =
      char '{'
      *> lift2
           (fun joltage j -> joltage @ [ j ])
           (many
              (take_while1 (function '0' .. '9' -> true | _ -> false)
              >>| Int.of_string <* char ','))
           (take_while1 (function '0' .. '9' -> true | _ -> false)
           >>| Int.of_string)
      <* char '}'
    in

    lift3
      (fun target buttons joltage ->
        { target_lights = target; buttons; target_joltage = joltage })
      target buttons joltage

  let of_string s =
    Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith

  let%expect_test _ =
    let t = of_string "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" in
    print_s [%sexp (t : t)];
    [%expect {| ((target 6) (buttons (8 10 4 12 5 3)) (joltage (3 5 4 7))) |}]

  let solve_lights t =
    let q = Queue.create () in
    let d = Int.Table.create () in
    let rec process_queue () =
      match Queue.dequeue q with
      | None -> ()
      | Some node when node = t.target_lights -> ()
      | Some node ->
          List.iter t.buttons ~f:(fun button ->
              let to_ = node lxor button in
              match Hashtbl.find d to_ with
              | None ->
                  Hashtbl.set d ~key:to_ ~data:(1 + Hashtbl.find_exn d node);
                  Queue.enqueue q to_
              | Some _ -> ());
          process_queue ()
    in
    Hashtbl.set d ~key:0 ~data:0;
    Queue.enqueue q 0;
    process_queue ();
    Hashtbl.find_exn d t.target_lights

  let solve_joltage t =
    let all_combos =
      let combos = Int.Table.create () in
      let rec get_all_combos curr buttons = function
        | [] ->
            Hashtbl.update combos curr ~f:(function
              | None -> [ buttons ]
              | Some existing -> buttons :: existing)
        | button :: tl ->
            get_all_combos curr buttons tl;
            get_all_combos (curr lxor button) (button :: buttons) tl
      in
      get_all_combos 0 [] t.buttons;
      combos
    in
    let solve_joltage =
      Memo.recursive ~hashable:Joltage.hashable (fun self joltage ->
          if List.for_all joltage ~f:(fun x -> x = 0) then Some 0
          else
            let last_unit_state =
              List.rev joltage
              |> List.fold ~init:0 ~f:(fun acc joltage ->
                  (acc * 2) + (joltage % 2))
            in
            Hashtbl.find all_combos last_unit_state
            |> ( function None -> [] | Some l -> l )
            |> List.filter_map ~f:(fun buttons ->
                let next_joltage =
                  List.fold buttons ~init:joltage ~f:(fun joltage button ->
                      List.mapi joltage ~f:(fun i v ->
                          if (button lsr i) land 1 = 1 then v - 1 else v))
                in
                match List.exists next_joltage ~f:(fun x -> x < 0) with
                | true -> None
                | false ->
                    assert (List.for_all next_joltage ~f:(fun x -> x % 2 = 0));
                    Option.map
                      (self (List.map next_joltage ~f:(fun x -> x / 2)))
                      ~f:(fun x -> (x * 2) + List.length buttons))
            |> List.min_elt ~compare:Int.compare)
    in
    solve_joltage t.target_joltage
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:Machine.of_string
  |> List.sum (module Int) ~f:Machine.solve_lights
  |> fun x -> print_endline (Int.to_string x)

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:Machine.of_string
  |> List.map ~f:Machine.solve_joltage
  |> List.sum (module Int) ~f:Option.value_exn
  |> fun x -> print_endline (Int.to_string x)
