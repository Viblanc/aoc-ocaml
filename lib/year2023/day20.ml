module SMap = Map.Make (String)

type pulse = High | Low

type mod_kind =
  | Broadcaster
  | FlipFlop of bool
  | Conjunction of (pulse * int) SMap.t

type com_mod = mod_kind * string list

let mod_of_string s =
  let l = Str.split (Str.regexp " -> ") s in
  let m, dst_mods =
    (List.hd l, List.tl l |> List.hd |> Str.split (Str.regexp ", "))
  in
  match m with
  | "broadcaster" -> (m, (Broadcaster, dst_mods))
  | _ -> (
      let name = String.sub m 1 (String.length m - 1) in
      match m.[0] with
      | '%' -> (name, (FlipFlop false, dst_mods))
      | '&' -> (name, (Conjunction SMap.empty, dst_mods))
      | _ -> failwith "Something unexpected happened")

type input = com_mod SMap.t

let parse filename =
  let add_input m inp mods =
    match SMap.find_opt m mods with
    | Some (Conjunction i, o) ->
        SMap.add m (Conjunction (SMap.add inp (Low, 0) i), o) mods
    | _ -> mods
  in
  (* name module modules *)
  let patch n m mods =
    let _, outputs = m in
    List.fold_left (fun m o -> add_input o n m) mods outputs
  in
  let modules =
    Utils.read_file_as_lines filename |> List.map mod_of_string |> SMap.of_list
  in
  SMap.fold patch modules modules

let push_button n (l, h) mods =
  let broadcast f o p = List.map (fun o -> (f, o, p)) o in
  let update (l, h) n = function Low -> (l + n, h) | High -> (l, h + n) in
  let rec send_pulse mods lh = function
    | [] -> (mods, lh)
    | (f, m, p) :: stack -> (
        match (SMap.find_opt m mods, p) with
        | None, _ -> send_pulse mods lh stack
        | Some (Broadcaster, o), p ->
            send_pulse mods
              (update lh (List.length o) p)
              (stack @ broadcast m o p)
        | Some (FlipFlop _, _), High -> send_pulse mods lh stack
        | Some (FlipFlop s, o), Low ->
            let s' = not s in
            let p' = if s then Low else High in
            let mods = SMap.add m (FlipFlop s', o) mods in
            send_pulse mods
              (update lh (List.length o) p')
              (stack @ broadcast m o p')
        | Some (Conjunction i, o), p ->
            let i =
              SMap.update f
                (function
                  | None -> Some (p, if p = High then n else 0)
                  | Some (_, n') -> Some (p, if p = High then n else n'))
                i
            in
            let p' =
              if SMap.for_all (fun _ (p, _) -> p = High) i then Low else High
            in
            let mods = SMap.add m (Conjunction i, o) mods in
            send_pulse mods
              (update lh (List.length o) p')
              (stack @ broadcast m o p'))
  in
  send_pulse mods (l + 1, h) [ ("button", "broadcaster", Low) ]

let part_1 input =
  let rec repeat n mods lh =
    match n with
    | 0 -> fst lh * snd lh
    | _ ->
        let mods, lh = push_button n lh mods in
        repeat (n - 1) mods lh
  in
  repeat 1000 input (0, 0) |> Answer.of_int

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let part_2 input =
  let complete v = SMap.for_all (fun _ (_, n) -> n > 0) v in
  let rec press n mods =
    let mods, _ = push_button n (0, 0) mods in
    match SMap.find "tg" mods with
    | Conjunction v, _ -> if complete v then v else press (n + 1) mods
    | _ -> failwith "impossible."
  in
  let s = press 1 input in
  SMap.fold (fun _ (_, a) acc -> lcm acc a) s 1 |> Answer.of_int
