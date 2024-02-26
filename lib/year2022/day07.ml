module Tree = struct
  type t = { name : string; size : int; parent : t option; children : t list }

  let add_node name node =
    {
      node with
      children =
        { name; size = 0; parent = None; children = [] } :: node.children;
    }

  let change_dir dir node =
    let child = List.find (fun n -> n.name = dir) node.children in
    { child with parent = Some node }

  let goto_parent node =
    match node.parent with
    | None -> node
    | Some parent ->
        {
          parent with
          size = parent.size + node.size;
          children =
            node :: List.filter (fun n -> n.name <> node.name) parent.children;
        }

  let from_input lines =
    let rec to_root node =
      let parent = goto_parent node in
      if parent.name = node.name then parent else to_root parent
    in
    List.fold_left
      (fun node line ->
        match String.split_on_char ' ' line with
        | [ "$"; "ls" ] -> node
        | [ "$"; "cd"; ".." ] -> goto_parent node
        | [ "$"; "cd"; dir ] -> change_dir dir node
        | [ "dir"; dir ] -> add_node dir node
        | [ size; _ ] -> { node with size = node.size + int_of_string size }
        | _ -> failwith "should not happen")
      { name = "/"; size = 0; parent = None; children = [] }
      (List.tl lines)
    |> to_root

  let rec filter f node =
    List.fold_left
      (fun acc child -> acc @ filter f child)
      (if f node then [ node ] else [])
      node.children
end

type input = Tree.t

let parse filename = Utils.read_file_as_lines filename |> Tree.from_input

let part_1 input =
  Tree.filter (fun node -> node.size <= 100_000) input
  |> List.fold_left (fun acc (node : Tree.t) -> acc + node.size) 0
  |> Answer.of_int

let part_2 (input : Tree.t) =
  let free_space = 70_000_000 - input.size in
  let needed_space = 30_000_000 - free_space in
  Tree.filter (fun node -> node.size >= needed_space) input
  |> List.fold_left (fun acc (node : Tree.t) -> min acc node.size) Int.max_int
  |> Answer.of_int
