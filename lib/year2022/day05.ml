type move = { n : int; src : int; dst : int }
type input = char list array * move list

let parse filename =
  let parse_stacks stacks =
    String.split_on_char '\n' stacks
    |> List.rev |> List.tl |> List.map String.to_seq |> List.to_seq
    |> Seq.transpose |> List.of_seq |> List.map List.of_seq
    |> List.map
         (List.filter (fun c -> match c with 'A' .. 'Z' -> true | _ -> false))
    |> List.filter (fun l -> List.length l <> 0)
    |> List.map List.rev |> Array.of_list
  in
  let parse_moves moves =
    let moves = Utils.get_lines moves in
    let scan n src dst = { n; src = src - 1; dst = dst - 1 } in
    List.map (fun s -> Scanf.sscanf s "move %d from %d to %d" scan) moves
  in
  let l = Utils.read_file filename |> Str.split (Str.regexp "\n\n") in
  let stacks = List.hd l in
  let moves = List.tl l |> List.hd in
  (parse_stacks stacks, parse_moves moves)

let split n l =
  let rec aux acc = function
    | -1, _ -> raise (Invalid_argument "n cannot be negative")
    | 0, t -> (acc, t)
    | _, [] -> (acc, [])
    | n, h :: t -> aux (h :: acc) (n - 1, t)
  in
  aux [] (n, l)

let move_crates stacks move =
  let { n; src; dst } = move in
  let src_stack = stacks.(src) in
  let dst_stack = stacks.(dst) in
  let moved, unmoved = split n src_stack in
  stacks.(src) <- unmoved;
  stacks.(dst) <- moved @ dst_stack;
  stacks

let part_1 input =
  let stacks, moves = input in
  List.fold_left move_crates stacks moves
  |> Array.map List.hd |> Array.map Char.escaped |> Array.fold_left ( ^ ) ""
  |> Answer.of_string

let move_crates stacks move =
  let { n; src; dst } = move in
  let src_stack = stacks.(src) in
  let dst_stack = stacks.(dst) in
  let moved, unmoved = split n src_stack in
  stacks.(src) <- unmoved;
  stacks.(dst) <- List.rev moved @ dst_stack;
  stacks

let part_2 input =
  let stacks, moves = input in
  List.fold_left move_crates stacks moves
  |> Array.map List.hd |> Array.map Char.escaped |> Array.fold_left ( ^ ) ""
  |> Answer.of_string
