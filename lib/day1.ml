open Core

module Instruction = struct
  module Rotation = struct
    type t = L | R
  end
  type t = {
    move: Rotation.t
            ; count: int
  }

  let of_string s =
    if String.is_prefix s ~prefix:"L" then
      {move = L; count = String.drop_prefix s 1 |>  Int.of_string}
    else if String.is_prefix s ~prefix:"R" then
      {move = R; count = String.drop_prefix s 1 |>  Int.of_string}
    else failwith ("Unexpected input " ^ s)
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:Instruction.of_string
  |> List.fold ~init:(50, 0) ~f:(fun (pos, ans) (instruction: Instruction.t) ->
      let pos = match instruction.move with
        | L -> ((pos - instruction.count) % 100 + 100) % 100
        | R -> ((pos + instruction.count) % 100) % 100
               in
               pos, ans + (if pos = 0 then 1 else 0)
    ) |> (fun (_, ans) -> print_endline (Int.to_string ans))
let part2 () =

  In_channel.input_lines In_channel.stdin
  |> List.map ~f:Instruction.of_string
  |> List.fold ~init:(50, 0) ~f:(fun (pos, ans) (instruction: Instruction.t) ->
      match instruction.move with
        | L -> ((pos - instruction.count) % 100 + 100) % 100, ans + (instruction.count / 100) + (if instruction.count % 100 >= pos && pos <> 0 then 1 else 0)
        | R -> ((pos + instruction.count) % 100) % 100, ans +  (instruction.count / 100) + (if instruction.count % 100 >= 100 - pos then 1 else 0)
  ) |> (fun (_, ans) -> print_endline (Int.to_string ans))
