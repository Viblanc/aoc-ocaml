type monkey = { items : int list; operation : int -> int; test : int -> int }
type input = monkey array

let parse filename =
  let parse_monkey arr =
    let to_int c = int_of_char c - int_of_char '0' in
    let scan_items items =
      Str.split (Str.regexp ", ") items |> List.map int_of_string
    in
    let scan_op fst_op op snd_op =
      let op =
        match op with
        | '+' -> Int.add
        | '-' -> Int.sub
        | '*' -> Int.mul
        | _ -> failwith "not supported"
      in
      if fst_op = snd_op then fun n -> op n n
      else fun n -> op n (int_of_string snd_op)
    in
    let scan_test n i = i mod n = 0 in
    let items = Scanf.sscanf arr.(0) "Starting items: %s@\n" scan_items in
    let operation =
      Scanf.sscanf arr.(1) "Operation: new = %s %c %s@\n" scan_op
    in
    let test =
      let func = Scanf.sscanf arr.(2) "Test: divisible by %d" scan_test in
      let monkey_true = to_int arr.(3).[String.length arr.(3) - 1] in
      let monkey_false = to_int arr.(4).[String.length arr.(4) - 1] in
      fun n -> if func n then monkey_true else monkey_false
    in
    { items; operation; test }
  in
  Utils.read_file filename
  |> Str.split (Str.regexp "\n\n")
  |> List.map (fun s ->
         String.split_on_char '\n' s
         |> List.tl
         |> List.filter (fun s -> String.length s <> 0)
         |> List.map String.trim |> Array.of_list)
  |> List.map parse_monkey |> Array.of_list

let part_1 _ = Answer.Int 0
let part_2 _ = Answer.Int 0
