type input = char Seq.t Seq.t list

let parse filename =
  Utils.read_file filename
  |> Str.split (Str.regexp "\n\n")
  |> List.map (String.split_on_char '\n')
  |> List.map (List.filter (fun s -> String.length s <> 0))
  |> List.map (fun l -> List.map String.to_seq l |> List.to_seq)

let find_mirror pattern =
  let find_m pat =
    let len = Seq.length pat in
    let arr = Array.of_seq pat in
    let rec is_mirror = function
      | x, y when x < 0 || y >= len -> true
      | x, y ->
          if Seq.for_all2 ( = ) arr.(x) arr.(y) then is_mirror (x - 1, y + 1)
          else false
    in
    let rec aux = function
      | i when i < len - 1 ->
          if is_mirror (i, i + 1) then Some (i + 1) else aux (i + 1)
      | _ -> None
    in
    aux 0
  in
  match find_m pattern with
  | Some i -> 100 * i
  | None -> Option.value (find_m (Seq.transpose pattern)) ~default:0

let part_1 input =
  List.map find_mirror input |> List.fold_left ( + ) 0 |> Answer.of_int

let find_mirror pattern =
  let find_m pat =
    let len = Seq.length pat in
    let arr = Array.of_seq pat in
    let count_smudges s s' =
      Seq.fold_left2 (fun acc a b -> acc + Bool.to_int (a <> b)) 0 s s'
    in
    let rec is_mirror smudge_found = function
      | x, y when x < 0 || y >= len -> smudge_found
      | x, y -> (
          match count_smudges arr.(x) arr.(y) with
          | 0 -> is_mirror smudge_found (x - 1, y + 1)
          | 1 -> if smudge_found then false else is_mirror true (x - 1, y + 1)
          | _ -> false)
    in
    let rec aux = function
      | i when i < len - 1 ->
          if is_mirror false (i, i + 1) then Some (i + 1) else aux (i + 1)
      | _ -> None
    in
    aux 0
  in
  match find_m pattern with
  | Some i -> 100 * i
  | None -> Option.value (find_m (Seq.transpose pattern)) ~default:0

let part_2 input =
  List.map find_mirror input |> List.fold_left ( + ) 0 |> Answer.of_int
