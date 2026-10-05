open! Core

module Gift = struct
  let of_input l =
    let t = Array.init 3 ~f:(fun _ -> Array.init 3 ~f:(Fn.const 0)) in
    let for_t, tl = List.split_n l 5 in
    List.drop for_t 1
    |> (fun for_t -> List.take for_t 3)
    |> List.iteri ~f:(fun i l ->
        String.iteri l ~f:(fun j -> function '#' -> t.(i).(j) <- 1 | _ -> ()));

    (t, tl)

  let count_occupied t =
    Array.sum (module Int) t ~f:(Array.sum (module Int) ~f:Fn.id)
end

module Region = struct
  type t = { n : int; m : int; gift_counts : int array }

  let t_parser =
    let open Angstrom in
    lift3
      (fun n m gift_counts -> { n; m; gift_counts })
      (take_while1 (function '0' .. '9' -> true | _ -> false)
      >>| Int.of_string)
      (char 'x'
      *> (take_while1 (function '0' .. '9' -> true | _ -> false)
         >>| Int.of_string))
      (char ':'
      *> (many
            (char ' '
             *> take_while1 (function '0' .. '9' -> true | _ -> false)
            >>| Int.of_string)
         >>| List.to_array))

  let of_string s =
    Angstrom.parse_string t_parser ~consume:All s |> Result.ok_or_failwith

  let area { n; m; gift_counts = _ } = n * m

  let count_gifts { n = _; m = _; gift_counts } =
    Array.sum (module Int) gift_counts ~f:Fn.id

  let count_occupied_for_gifts { n = _; m = _; gift_counts } gifts =
    Array.mapi gift_counts ~f:(fun i count ->
        count * Gift.count_occupied gifts.(i))
    |> Array.sum (module Int) ~f:Fn.id
end

let parse_input input_lines =
  let input_lines, gifts =
    List.init 6 ~f:(Fn.const ())
    |> List.fold_map ~init:input_lines ~f:(fun lines () ->
        let gift, lines = Gift.of_input lines in
        (lines, gift))
  in
  let regions = List.map input_lines ~f:Region.of_string in
  (Array.of_list gifts, regions)

let part1 () =
  let gifts, regions = In_channel.input_lines In_channel.stdin |> parse_input in

  List.sum
    (module Int)
    regions
    ~f:(fun region ->
      let area = Region.area region in
      let count_gifts = Region.count_gifts region in
      let count_occupied = Region.count_occupied_for_gifts region gifts in
      let percentage = Float.of_int area /. Float.of_int count_gifts in
      if Float.( >= ) percentage 9. then 1
      else if count_occupied >= area then 0
      else
        raise_s
          [%message
            "not trivially regectable"
              (percentage : float)
              (area : int)
              (count_occupied : int)
              (count_gifts : int)])
  |> fun x -> print_endline (Int.to_string x)

let part2 () = ()
