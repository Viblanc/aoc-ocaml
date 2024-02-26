type input = (int * (int * int * int)) list

(* We don't actually need to count the cubes from each set for each game
   We just need to retrieve the highest quantity of cubes for each color *)

let parse_line line =
  let parse_cubes n = function
    | "red" -> (n, 0, 0)
    | "green" -> (0, n, 0)
    | "blue" -> (0, 0, n)
    | _ -> failwith "Unexpected color"
  in
  let parse_sets id sets =
    let cubes = Str.split (Str.regexp {|[;,] |}) sets in
    (id, List.map (fun s -> Scanf.sscanf s "%d %s" parse_cubes) cubes)
  in
  let id, cubes = Scanf.sscanf line "Game %d: %s@\n" parse_sets in
  ( id,
    List.fold_left
      (fun (r, g, b) (r', g', b') -> (max r r', max g g', max b b'))
      (0, 0, 0) cubes )

let parse filename = Utils.read_file_as_lines filename |> List.map parse_line
let game_is_possible (r, g, b) = r <= 12 && g <= 13 && b <= 14

let part_1 input =
  List.filter_map
    (fun (id, game) -> if game_is_possible game then Some id else None)
    input
  |> List.fold_left ( + ) 0 |> Answer.of_int

let part_2 input =
  List.fold_left (fun acc (_, (r, g, b)) -> acc + (r * g * b)) 0 input
  |> Answer.of_int
