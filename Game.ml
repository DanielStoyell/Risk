open Buildings
open Player

type phase = Init of (string*int) list | Dist of int | Attack of bool | Fort

type game = {
  phase : phase;
  buildings : building list;
  players : player list;
  cTurn : string
}

(* Determines if there is a path between the two buildings passed in.*)
let has_path game (n1:building) (n2:building) =
  let rec dfs q v =
    match q with
    | [] -> false
    | h::t -> if h = n2.name then true else
      let valid b = not (List.exists (fun e -> e=b) v) &&
      ((List.find (fun (e:building) -> e.name = b) game.buildings).owner =
      game.cTurn) in
      let build = List.find (fun (e:building) -> e.name = h) game.buildings in
      let neighbors = build.neighbors in
      let to_add = List.filter valid neighbors in
      let v = v @ to_add in
      dfs (t @ to_add) v in
  let visited = [n1.name] in
  let queue = [n1.name] in
  dfs queue visited

(* Determines the amount of troops the player currently has.*)
let get_player_troops (p:player) (game:game) =
  let get_troops b = (string_to_build b game.buildings).troops in
  List.fold_left (fun a b -> a + (get_troops b)) 0 p.buildings