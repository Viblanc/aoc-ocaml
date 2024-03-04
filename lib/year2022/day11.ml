type monkey = {
  items : int Queue.t;
  operation : int -> int;
  test_value : int;
  if_true : int;
  if_false : int;
}

type input = string array list

let parse_monkeys lines =
  let parse_monkey arr =
    let to_int c = int_of_char c - int_of_char '0' in
    let scan_items items =
      Str.split (Str.regexp ", ") items |> List.map int_of_string |> List.to_seq
    in
    let scan_op fst_op op snd_op =
      let op =
        match op with
        | '+' -> Int.add
        | '*' -> Int.mul
        | _ -> failwith "not supported"
      in
      if fst_op = snd_op then fun n -> op n n
      else fun n -> op n (int_of_string snd_op)
    in
    let scan_test n = n in
    let items =
      Queue.of_seq (Scanf.sscanf arr.(0) "Starting items: %s@\n" scan_items)
    in
    let operation =
      Scanf.sscanf arr.(1) "Operation: new = %s %c %s@\n" scan_op
    in
    let test_value = Scanf.sscanf arr.(2) "Test: divisible by %d" scan_test in
    let if_true = to_int arr.(3).[String.length arr.(3) - 1] in
    let if_false = to_int arr.(4).[String.length arr.(4) - 1] in
    { items; operation; test_value; if_true; if_false }
  in
  List.map parse_monkey lines |> Array.of_list

let parse filename =
  Utils.read_file filename
  |> Str.split (Str.regexp "\n\n")
  |> List.map (fun s ->
         String.split_on_char '\n' s
         |> List.tl
         |> List.filter (fun s -> String.length s <> 0)
         |> List.map String.trim |> Array.of_list)

let run_round monkeys scores f =
  let rec inspect_item idx monkey =
    match Queue.take monkey.items with
    | exception Queue.Empty -> ()
    | item ->
        scores.(idx) <- scores.(idx) + 1;
        let item = f (monkey.operation item) in
        let nxt =
          if item mod monkey.test_value = 0 then monkey.if_true
          else monkey.if_false
        in
        Queue.add item monkeys.(nxt).items;
        inspect_item idx monkeys.(idx)
  in
  Array.iteri inspect_item monkeys;
  (monkeys, scores)

let rec repeat monkeys scores f = function
  | 0 -> scores
  | n ->
      let monkeys, scores = run_round monkeys scores f in
      repeat monkeys scores f (n - 1)

let part_1 input =
  let monkeys = parse_monkeys input in
  let scores =
    repeat monkeys
      (Array.init (Array.length monkeys) (fun _ -> 0))
      (fun n -> n / 3)
      20
  in
  Array.sort (Fun.flip compare) scores;
  Answer.Int (scores.(0) * scores.(1))

let part_2 input =
  let monkeys = parse_monkeys input in
  let lcm =
    Array.map (fun m -> m.test_value) monkeys |> Array.fold_left Int.mul 1
  in
  let scores =
    repeat monkeys
      (Array.init (Array.length monkeys) (fun _ -> 0))
      (fun n -> n mod lcm)
      10_000
  in
  Array.sort (Fun.flip compare) scores;
  Answer.Int (scores.(0) * scores.(1))
