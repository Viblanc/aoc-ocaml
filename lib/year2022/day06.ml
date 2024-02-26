type input = char Seq.t

let parse filename = Utils.read_file filename |> String.to_seq

let group_by_4 l =
  if List.length l < 4 then failwith "list too small"
  else
    let rec aux acc = function
      | [ _; _; _ ] -> List.rev acc
      | a :: (b :: c :: d :: _ as t) -> aux ((a, b, c, d) :: acc) t
      | _ -> failwith "should not happen"
    in
    aux [] l

let find_first_marker l =
  let is_marker (a, b, c, d) =
    (a <> b && a <> c && a <> d) && (b <> c && b <> d) && c <> d
  in
  List.mapi (fun i t -> (i + 4, t)) l
  |> List.find (fun (_, t) -> is_marker t)
  |> fst |> Answer.of_int

let part_1 input = List.of_seq input |> group_by_4 |> find_first_marker

module CS = Set.Make (Char)

let rec find_first_msg_marker seq i n =
  let set = CS.of_seq (Seq.take n seq) in
  if CS.cardinal set = n then i + n
  else find_first_msg_marker (Seq.drop 1 seq) (i + 1) n

let part_2 input = Answer.Int (find_first_msg_marker input 0 14)
