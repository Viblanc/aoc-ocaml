type packet = Num of int | Pack of packet list

let rec compare_packets p p' =
  match (p, p') with
  | Num n, Num n' -> Int.compare n n'
  | Num _, _ -> compare_packets (Pack [ p ]) p'
  | Pack [], Pack [] -> 0
  | Pack [], _ -> -1
  | _, Pack [] -> 1
  | Pack (h :: t), Pack (h' :: t') -> (
      match compare_packets h h' with
      | 0 -> compare_packets (Pack t) (Pack t')
      | n -> n)
  | _ -> -1 * compare_packets p' p

type input = (packet * packet) list

let packet_of_string s =
  let get_int l =
    let seq = List.to_seq l in
    let s =
      Seq.take_while (function '0' .. '9' -> true | _ -> false) seq
      |> String.of_seq
    in
    (Seq.drop (String.length s) seq |> List.of_seq, int_of_string s)
  in
  let rec aux acc = function
    | '[' :: l -> (
        let acc', ll = aux [] l in
        let acc = Pack acc' :: acc in
        match ll with [] -> (acc, []) | _ -> aux acc ll)
    | ']' :: l -> (List.rev acc, l)
    | ',' :: l -> aux acc l
    | l ->
        let l, n = get_int l in
        aux (Num n :: acc) l
  in
  List.hd (fst (aux [] (String.to_seq s |> List.of_seq)))

let parse filename =
  Utils.read_file filename
  |> Str.split (Str.regexp "\n\n")
  |> List.map (String.split_on_char '\n')
  |> List.map (function
       | a :: b :: _ -> (packet_of_string a, packet_of_string b)
       | _ -> failwith "should not happen")

let part_1 input =
  List.mapi
    (fun i (p, p') -> if compare_packets p p' = -1 then i + 1 else 0)
    input
  |> List.fold_left ( + ) 0 |> Answer.of_int

let part_2 input =
  let key_1, key_2 = (Pack [ Pack [ Num 2 ] ], Pack [ Pack [ Num 6 ] ]) in
  key_1 :: key_2 :: (List.map (fun (p, p') -> [ p; p' ]) input |> List.flatten)
  |> List.sort compare_packets
  |> List.mapi (fun i p -> if p = key_1 || p = key_2 then i + 1 else 1)
  |> List.fold_left Int.mul 1 |> Answer.of_int
