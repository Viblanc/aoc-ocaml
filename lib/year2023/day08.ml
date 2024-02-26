type input = (string, string * string) Hashtbl.t * char Seq.t

let parse_network lines =
  let parse_line line =
    let parse key left right = (key, (left, right)) in
    Scanf.sscanf line "%s = (%s@, %s@)" parse
  in
  List.map parse_line lines |> List.to_seq |> Hashtbl.of_seq

let parse filename =
  let l = Utils.read_file filename |> Str.split (Str.regexp "\n\n") in
  let instructions = List.hd l |> String.to_seq |> Seq.cycle in
  let network =
    parse_network
      (List.tl l |> List.hd |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s <> 0))
  in
  (network, instructions)

let rec count_steps network instructions cur is_goal =
  if is_goal cur then 0
  else
    let left, right = Hashtbl.find network cur in
    match instructions () with
    | Seq.Cons ('L', s) -> 1 + count_steps network s left is_goal
    | Seq.Cons ('R', s) -> 1 + count_steps network s right is_goal
    | _ -> failwith "Should not happen!"

let part_1 input =
  let network, instructions = input in
  count_steps network instructions "AAA" (String.equal "ZZZ") |> Answer.of_int

let find_starting_nodes network =
  Hashtbl.to_seq network |> List.of_seq
  |> List.filter (fun (k, _) -> String.ends_with ~suffix:"A" k)
  |> List.map fst

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let part_2 input =
  let network, instructions = input in
  find_starting_nodes network
  |> List.map (fun k ->
         count_steps network instructions k (String.ends_with ~suffix:"Z"))
  |> List.fold_left (fun acc c -> Int.max c (lcm acc c)) 0
  |> Answer.of_int
