type hand = Rock | Paper | Scissors

let score_of_hand = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let hand_of_char = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | _ -> Scissors

let rec compare_hands = function
  | Rock, Paper -> -1
  | Rock, Scissors -> 1
  | Paper, Scissors -> -1
  | h1, h2 -> if h1 = h2 then 0 else -1 * compare_hands (h2, h1)

type input = (char * char) list

let parse filename =
  let scan_line c c' = (c, c') in
  Utils.read_file_as_lines filename
  |> List.map (fun s -> Scanf.sscanf s "%c %c" scan_line)

let score_of_game = function -1 -> 6 | 1 -> 0 | _ -> 3

let part_1 input =
  let input =
    List.map (fun (c, c') -> (hand_of_char c, hand_of_char c')) input
  in
  let hands =
    List.map snd input |> List.map score_of_hand |> List.fold_left ( + ) 0
  in
  let games =
    List.map compare_hands input
    |> List.map score_of_game |> List.fold_left ( + ) 0
  in
  Answer.of_int (hands + games)

type game = Win | Draw | Lose

let game_of_char = function 'X' -> Lose | 'Y' -> Draw | _ -> Win

let rec get_right_hand = function
  | h, Draw -> h
  | Rock, Lose -> Scissors
  | Paper, Lose -> Rock
  | Scissors, Lose -> Paper
  | h, Win -> get_right_hand (get_right_hand (h, Lose), Lose)

let score_of_game = function Win -> 6 | Lose -> 0 | Draw -> 3

let part_2 input =
  let input =
    List.map (fun (c, c') -> (hand_of_char c, game_of_char c')) input
  in
  let games =
    List.map snd input |> List.map score_of_game |> List.fold_left ( + ) 0
  in
  let hands =
    List.map get_right_hand input
    |> List.map score_of_hand |> List.fold_left ( + ) 0
  in
  Answer.of_int (hands + games)
