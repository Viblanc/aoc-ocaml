type part = { x : int; m : int; a : int; s : int }
type rule = { target : char; cond : int -> bool; dest : string }
type workflow = string * rule list
type input = (string, rule list) Hashtbl.t * part list

let parse input =
  let parse_workflow w =
    let scan_rule target sign n dest =
      let cond k = match sign with '<' -> k < n | _ -> k > n in
      { target; cond; dest }
    in
    let parse_rule rule =
      match Scanf.sscanf_opt rule "%c%c%d:%s" scan_rule with
      | Some r -> r
      | None -> { target = 'o'; cond = (fun _ -> true); dest = rule }
    in
    let scan_workflow name rules =
      let rules = String.split_on_char ',' rules in
      (name, List.map parse_rule rules)
    in
    Scanf.sscanf w "%s@{%s@}" scan_workflow
  in
  let parse_part part =
    let f x m a s = { x; m; a; s } in
    Scanf.sscanf part "{x=%d,m=%d,a=%d,s=%d" f
  in
  let l = Str.split (Str.regexp "\n\n") input in
  let workflows = List.hd l |> String.split_on_char '\n' in
  let parts =
    List.tl l |> List.hd |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s <> 0)
  in
  ( List.map parse_workflow workflows |> List.to_seq |> Hashtbl.of_seq,
    List.map parse_part parts )

let rec follow_workflow part rules workflow_tbl =
  let { x; a; m; s } = part in
  let get_category = function
    | 'x' -> x
    | 'm' -> m
    | 'a' -> a
    | 's' -> s
    | _ -> 0
  in
  match rules with
  | [] -> failwith "Should not happen!"
  | { target; cond; dest } :: rules -> (
      let category = get_category target in
      match cond category with
      | false -> follow_workflow part rules workflow_tbl
      | true -> (
          match dest with
          | "A" -> Some (x + m + a + s)
          | "R" -> None
          | _ ->
              let new_rules = Hashtbl.find workflow_tbl dest in
              follow_workflow part new_rules workflow_tbl))

let part_1 input =
  let workflows, parts = input in
  let starting_workflow = Hashtbl.find workflows "in" in
  List.filter_map (fun p -> follow_workflow p starting_workflow workflows) parts
  |> List.fold_left ( + ) 0 |> Answer.of_int

type part_seq = { x : int Seq.t; m : int Seq.t; a : int Seq.t; s : int Seq.t }

let rec follow_workflow part rules workflow_tbl =
  let { x; m; a; s } = part in
  let get_rules key = Hashtbl.find workflow_tbl key in
  let get_combinations { x; m; a; s } =
    Seq.length x * Seq.length m * Seq.length a * Seq.length s
  in
  let filter cond = function
    | 'x' -> { part with x = Seq.filter cond x }
    | 'm' -> { part with m = Seq.filter cond m }
    | 'a' -> { part with a = Seq.filter cond a }
    | _ -> { part with s = Seq.filter cond s }
  in
  match rules with
  | [] -> 0
  | { target; cond; dest } :: rs -> (
      let part_true = filter cond target in
      let part_false = filter (Fun.negate cond) target in
      match dest with
      | "A" ->
          get_combinations part_true
          + follow_workflow part_false rs workflow_tbl
      | "R" -> follow_workflow part_false rs workflow_tbl
      | _ ->
          follow_workflow part_true (get_rules dest) workflow_tbl
          + follow_workflow part_false rs workflow_tbl)

let part_2 input =
  let workflows, _ = input in
  let starting_workflow = Hashtbl.find workflows "in" in
  let vals = Seq.ints 1 |> Seq.take 4000 in
  let part = { x = vals; m = vals; a = vals; s = vals } in
  follow_workflow part starting_workflow workflows |> Answer.of_int
