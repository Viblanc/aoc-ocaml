type input = string list

let parse input = Utils.read_file_as_lines input
let zero = int_of_char '0'
let to_int b = int_of_char b - zero

let find_digits str =
  let bytes = String.to_bytes str in
  let first, second =
    Bytes.fold_left
      (fun (f, s) b ->
        match b with
        (* If the first digit was not found, we replace it *)
        (* In any case, we always replace the second since we're looking for the latest digit *)
        | '0' .. '9' -> if f = -1 then (to_int b, to_int b) else (f, to_int b)
        | _ -> (f, s))
      (-1, 0) bytes
  in
  (first * 10) + second

let part_1 input =
  List.map find_digits input |> List.fold_left ( + ) 0 |> Answer.of_int

let get_digit (i, c) str =
  let digits =
    [|
      "zero";
      "one";
      "two";
      "three";
      "four";
      "five";
      "six";
      "seven";
      "eight";
      "nine";
    |]
  in
  let is_spelled_digit spelled_digit start =
    let len = String.length spelled_digit in
    let substr =
      (* We take the min to avoid a @Invalid_argument exception *)
      String.sub str start (Int.min len (String.length str - start))
    in
    spelled_digit = substr
  in
  match c with
  | '0' .. '9' -> Some (to_int c)
  | _ -> Array.find_index (fun d -> is_spelled_digit d i) digits

let find_digits str =
  (* We can't work by Bytes here since we need to manipulate indexes *)
  let chars = String.to_seq str |> List.of_seq in
  let first, second =
    List.mapi (fun i b -> (i, b)) chars
    |> List.fold_left
         (fun (f, s) b ->
           if Option.is_none f then (get_digit b str, get_digit b str)
           else
             match (s, get_digit b str) with
             | None, o -> (f, o)
             | Some d, None -> (f, Some d)
             | Some _, Some d' -> (f, Some d'))
         (None, None)
  in
  (Option.get first * 10) + Option.get second

let part_2 input =
  List.map find_digits input |> List.fold_left ( + ) 0 |> Answer.of_int
