type input = string list

let parse filename =
  Utils.read_file filename |> String.split_on_char ',' |> List.map String.trim

let hash s =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux ((acc + Char.code h) * 17 mod 256) t
  in
  aux 0 (List.of_seq (String.to_seq s))

let part_1 input =
  List.map hash input |> List.fold_left ( + ) 0 |> Answer.of_int

module Box = Map.Make (String)

let to_lens s =
  let len = String.length s in
  match s.[len - 1] with
  | '-' -> (String.split_on_char '-' s |> List.hd, None)
  | _ -> (
      match String.split_on_char '=' s with
      | [ label; f_len ] -> (label, Some (int_of_string f_len))
      | _ -> failwith "Should not happen!")

let get_lens_config lenses =
  let boxes = Array.make 256 Box.empty in
  let remove label =
    let id = hash label in
    let map = boxes.(id) in
    boxes.(id) <- Box.remove label map
  in
  let add label f_len idx =
    let id = hash label in
    let map = boxes.(id) in
    match Box.find_opt label map with
    | None -> boxes.(id) <- Box.add label (idx, f_len) map
    | Some (i, _) -> boxes.(id) <- Box.add label (i, f_len) map
  in
  let sort_lens i = function
    | label, None -> remove label
    | label, Some f_len -> add label f_len (i + 1)
  in
  List.iteri sort_lens lenses;
  boxes

let part_2 input =
  let cmp (n, _) (m, _) = compare n m in
  let boxes = get_lens_config (List.map to_lens input) in
  Array.mapi
    (fun i b ->
      Box.to_list b |> List.split |> snd |> List.sort cmp
      |> List.mapi (fun j (_, f) -> (i + 1) * (j + 1) * f)
      |> List.fold_left ( + ) 0)
    boxes
  |> Array.fold_left ( + ) 0 |> Answer.of_int
