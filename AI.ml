open Player
open Buildings
open Game


(* Return the list of enemy neighboring buildings of a building *)
let building_neighbors (build:building) (all_buildings:building list) : building list =
  let rec change_bors_to_build_lst build_lst acc =
  match build_lst with
  | [] -> acc
  | h::t -> change_bors_to_build_lst t ((string_to_build h all_buildings)::acc) in
  let neighbor_lst = change_bors_to_build_lst build.neighbors [] in
  List.filter (fun x -> x.owner <> build.owner) neighbor_lst

(* Returns a list of friendly neighbors of a building *)
let friendly_building_neighbors (build:building) (all_buildings:building list) : building list =
  let rec change_bors_to_build_lst build_lst acc =
  match build_lst with
  | [] -> acc
  | h::t -> change_bors_to_build_lst t ((string_to_build h all_buildings)::acc) in
  let neighbor_lst = change_bors_to_build_lst build.neighbors [] in
  List.filter (fun x -> x.owner = build.owner) neighbor_lst

(* Returns a value that represents how vulnerable a building is. Smaller values
 * represent more vulnerable buildings *)
let find_strength b game =
  let find_troops b =
    let b = string_to_build b game.buildings in
    if b.owner <> game.cTurn then b.troops else 0 in
  let b = string_to_build b game.buildings in
  let raw_vuln = (float_of_int (b.troops -
    (List.fold_left (fun a x -> a + (find_troops x)) 0 b.neighbors)))
    /. (float_of_int b.troops) in
  if (raw_vuln > 0.8) then raw_vuln +. (float_of_int b.troops) else raw_vuln

(* Sort a list of buildings by how vulnerable they are. If vuln_first is true
 * then sort most vulnerable to least vulnerable, else sort least to most
 * vulnerable *)
let sort_vulnerable builds game vuln_first =
  let lst = List.map (fun e -> (find_strength e game, e)) builds in
  let comp = if vuln_first then (fun a b -> int_of_float(fst a -. fst b))
             else (fun a b -> int_of_float(fst b -. fst a)) in
  let lst = List.sort comp lst in
  List.map (fun e -> snd e) lst

(* Returns on average how many troops an attacking player will lose in a given
 * encounter *)
let rec how_many_troops_lost start_att att def =
  if att = 0 then float_of_int start_att else
  if def = 0 then float_of_int (start_att - att) else
  if att = 1 then
    if def = 1 then
      (0.583 *. (how_many_troops_lost start_att (att - 1) def)) +.
      (0.417 *. (how_many_troops_lost start_att att (def - 1))) else
    (0.745 *. (how_many_troops_lost start_att (att - 1) def)) +.
    (0.255 *. (how_many_troops_lost start_att att (def - 1))) else
  if att = 2 then
    if def = 1 then
      (0.421 *. (how_many_troops_lost start_att (att - 1) def)) +.
      (0.579 *. (how_many_troops_lost start_att att (def - 1))) else
    (0.448 *. (how_many_troops_lost start_att (att - 2) def)) +.
    (0.228 *. (how_many_troops_lost start_att att (def - 2))) +.
    (0.324 *. (how_many_troops_lost start_att (att - 1) (def - 1))) else
  if def = 1 then
    (0.66 *. (how_many_troops_lost start_att (att - 1) def)) +.
    (0.34 *. (how_many_troops_lost start_att att (def - 1))) else
  (0.292 *. (how_many_troops_lost start_att (att - 2) def)) +.
  (0.372 *. (how_many_troops_lost start_att att (def - 2))) +.
  (0.336 *. (how_many_troops_lost start_att (att - 1) (def - 1)))

(* Returns how valuable a building is to a player attacking it. Higher values
 * represent a more valuable building. *)
let build_value att def game : float =
  let gAIN_REGION_WEIGHT = 2. in
  let sTOP_REGION_WEIGHT = 2. in
  let lOST_TROOPS_WEIGHT = 1. in
  let oVERALL_MOD        = 3. in
  let reg_name = att.region in
  let reg_buildings = List.filter (fun b -> b.region = reg_name) game.buildings in
  let owned = List.filter (fun b -> b.owner = def.owner) reg_buildings in
  let gain_region = (float_of_int (List.length owned))
                    /. (float_of_int(List.length reg_buildings)) in
  let stop_region = if List.for_all (fun b -> b.owner = att.owner) reg_buildings
    then 1. else 0. in
  let lost = 1.0 -. ((how_many_troops_lost att.troops att.troops def.troops)) /.
    (float_of_int (get_player_troops (string_to_player game.cTurn game.players) game)) in
  let result = ((
    (gain_region *. gAIN_REGION_WEIGHT) +.
    (stop_region *. sTOP_REGION_WEIGHT) +.
    (lost *.  lOST_TROOPS_WEIGHT))
    /. (gAIN_REGION_WEIGHT +. sTOP_REGION_WEIGHT +. lOST_TROOPS_WEIGHT))
    /. oVERALL_MOD in
  result

(* Given a list of buildings sorts the buildings by their region value to a
 * player. Higher region values correspond to a player owning more of the
 * region. *)
let sort_by_rvalue lst g =
  if lst = [] then [] else
  let player = Random.self_init (); string_to_player g.cTurn g.players in
  let find_rvalue b =
    let reg_buildings = List.filter (fun e -> b.region = e.region) g.buildings in
    let owned = List.filter (fun e -> e.owner = player.name) reg_buildings in
    (float_of_int (List.length owned)) /.
    (float_of_int (List.length reg_buildings)) in
  let tagged = List.map (fun e -> (find_rvalue e, e)) lst in
  let sorted = List.sort (fun a b -> compare (fst b) (fst a)) tagged in
  if fst (List.hd sorted) = 0. then
    let rand = List.map (fun e -> (Random.bits (), snd e)) sorted in
    let sorted = List.sort (fun a b -> compare (fst b) (fst a)) rand in
    List.map (fun e -> snd e) sorted
  else
    List.map (fun e -> snd e) sorted

(* Returns the building that is the most vulnerable of a player's buildings. *)
let rec where_to_place_troops players_builds all_builds : building =
  let names = List.map (fun e -> e.name) players_builds in
  let tempGame = {
      buildings = all_builds;
      players = [];
      cTurn = (List.hd players_builds).owner;
      phase = Attack false
  } in
  let vulns = sort_vulnerable names tempGame true in
  string_to_build (List.hd vulns) all_builds

(* Returns which building a player should place a troop on in the initial
 * distribution phase of the game. *)
let where_place_troops_begin (builds:building list) (playa:string) (g:game) : string =
  let ai_buildings = Random.self_init (); List.filter (fun x -> x.owner = playa) builds in
  let rec find_build buildins =
    match buildins with
    | [] -> (let unowned = List.filter (fun x -> x.owner = "") builds in
            if unowned = [] then let bld =
                    where_to_place_troops ai_buildings builds in
            let () = Printf.printf "%s placed a troop on %s \n" playa bld.name
              in bld.name else
            let unowned = sort_by_rvalue unowned g in
            match unowned with
            | [] -> failwith "Must have somewhere to place troops"
            | h1::_ -> let () =
                Printf.printf "%s placed a troop on %s \n" playa h1.name in h1.name)
    | lst -> (let neighbors b = strings_to_builds b.neighbors builds [] in
              let neighs = List.map (fun e -> neighbors e) lst in
              let neighs = List.flatten neighs in
              let unowned = List.filter (fun x -> x.owner = "") neighs in
              let unowned = sort_by_rvalue unowned g in
              match unowned with
              | [] -> find_build []
              | hd::_ -> let () = Printf.printf "%s placed a troop on %s \n"
                playa hd.name in hd.name) in
  find_build ai_buildings

(* Returns the probability that an attack will be successful given how many
 * troops are attacking and defending in the encounter. *)
let rec success_vs_failure att def =
  if att = 0 then 0. else
  if def = 0 then 1. else
  if att = 1 then
    if def = 1 then
      (0.583 *. (success_vs_failure (att - 1) def)) +.
      (0.417 *. (success_vs_failure att (def - 1))) else
    (0.745 *. (success_vs_failure (att - 1) def)) +.
    (0.255 *. (success_vs_failure att (def - 1))) else
  if att = 2 then
    if def = 1 then
      (0.421 *. (success_vs_failure (att - 1) def)) +.
      (0.579 *. (success_vs_failure att (def - 1))) else
    (0.448 *. (success_vs_failure (att - 2) def)) +.
    (0.228 *. (success_vs_failure att (def - 2))) +.
    (0.324 *. (success_vs_failure (att - 1) (def - 1))) else
  if def = 1 then
    (0.66 *. (success_vs_failure (att - 1) def)) +.
    (0.34 *. (success_vs_failure att (def - 1))) else
  (0.292 *. (success_vs_failure (att - 2) def)) +.
  (0.372 *. (success_vs_failure att (def - 2))) +.
  (0.336 *. (success_vs_failure (att - 1) (def - 1)))

(* A simple probability measure of whether the AI should attack. *)
let should_attack prob = prob > 0.7

(* If get below 15 troops or if have 10+ stars then turn in stars *)
let stars_trade_for_troops player_name playas builds =
  let playa = string_to_player player_name playas in
  if playa.stars < 2 then
    let () = Printf.printf "%s turned in 0 stars \n" player_name in 0 else
  if playa.stars > 9 then
    let () = Printf.printf "%s turned in 10 stars \n" player_name in 10 else
  let builds = strings_to_builds playa.buildings builds [] in
  let total_troops = List.fold_left (fun a x -> x.troops + a) 0 builds in
  if total_troops < 15 then
    let () = Printf.printf "%s turned in %d stars \n" player_name playa.stars in
  playa.stars else
    let () = Printf.printf "%s turned in 0 stars \n" player_name in 0

(* Returns a tuple that says whether a computer wants to fortify, which
 * building they want to fortify from, which building they want to fortify
 * to, and how many troops they want to move between the buildings. *)
let fortify_stuff g =
  let largest_neighbor b =
    let b = string_to_build b g.buildings in
    List.fold_left (fun a x -> let t = (string_to_build x g.buildings) in
      if t.owner = b.owner then a else
      if a > t.troops then a else t.troops) 1 b.neighbors in
  let p = string_to_player g.cTurn g.players in
  let vulns = sort_vulnerable p.buildings g false in
  let rec find_where_move_to vuln move_from =
    match vuln with
    | [] -> (move_from,0)
    | hd::tl -> let toTransfer = (string_to_build move_from g.buildings).troops
                - (largest_neighbor move_from) in if toTransfer <= 0 then
                find_where_move_to tl move_from else
                (hd,toTransfer) in
  let rec where_fortify vul =
    match vul with
    | []
    | _::[] -> ("","",0)
    | h::m::t -> let transf = find_where_move_to (List.rev(m::t)) h in
                 if snd(transf) = 0 then where_fortify (m::t) else
                 (h,fst(transf),snd(transf)) in
  let transfers = where_fortify vulns in
  match transfers with
  | (_,_,0) ->
    let () = Printf.printf "%s declined to fortify this turn. \n" g.cTurn in
    (false, List.hd g.buildings, List.hd g.buildings, 0)
  | (from,bTo,troops) ->
    let () = Printf.printf "%s will move %d troops from %s to %s. \n"
    g.cTurn troops from bTo in
    (true,string_to_build from g.buildings,string_to_build bTo g.buildings,troops)


(* Returns how many troops the computer should move onto a building after they
 * successfully took over a building. *)
let troops_to_move gme from_bld to_bld attack_with =
  let new_bors = to_bld.neighbors in
  let build_bors = strings_to_builds new_bors gme.buildings [] in
  let enemy_bors = List.filter (fun x -> x.owner <> gme.cTurn) build_bors in
  match enemy_bors with
  | [] ->
    let () = Printf.printf "%s moved %d troops from %s to %s after taking over %s \n"
    gme.cTurn attack_with from_bld.name to_bld.name to_bld.name in attack_with
  | _::_ ->
    let () = Printf.printf "%s moved %d troops from %s to %s after taking over %s \n"
    gme.cTurn (from_bld.troops - 1) from_bld.name to_bld.name to_bld.name in
    from_bld.troops - 1

(* Given a building to put troops on and how many total troops to distribute,
 * returns how many troops should be placed on the building. *)
let how_many_to_distribute gme (build_on:building) total_to_dist  =
  let largest_neighbor b =
    List.fold_left (fun a x -> let t = (string_to_build x gme.buildings) in
      if t.owner = b.owner then a else
      if a > t.troops then a else t.troops) 1 b.neighbors in
  let large_bor = largest_neighbor build_on in
  let to_move = large_bor - build_on.troops in
  if to_move > total_to_dist then total_to_dist else to_move

(* Given how many troops should be distributed (a number > 0), returns a pair
 * consisting of how many troops should be placed on a building and which
 * building those troops should be placed on. *)
let to_distribute gme to_distribute =
  let player_builds = List.filter (fun x -> x.owner = gme.cTurn) gme.buildings in
  let rec dist all_builds =
    match all_builds with
    | [] -> let build = where_to_place_troops player_builds gme.buildings in
            let d = (to_distribute, build.name) in
            let () = Printf.printf "%s placed %d troops on %s"
              gme.cTurn to_distribute (snd(d)) in d
    | _::_ -> let build = where_to_place_troops all_builds gme.buildings in
              let troops = how_many_to_distribute gme build to_distribute in
              if troops > 0 then
                let d = (troops, build.name) in
                let () = Printf.printf "%s placed %d troops on %s"
                  gme.cTurn troops (snd(d)) in d else
              dist (List.filter (fun x -> x.name <> build.name) all_builds) in
  dist player_builds

(* Returns what building the AI should place a troop on in the initial phase of
 * the game. *)
let aiInit gme : string =
  where_place_troops_begin gme.buildings gme.cTurn gme

(* Given a building to attack from, returns either the name of a building to
 * attack if the probability of an attack is high enough or None if there
 * is not a high enough probability of a successful attack. *)
let rec should_this_building_attack (gme:game) (attack_loc:building) : building option =
  let exits = strings_to_builds attack_loc.neighbors gme.buildings [] in
  let exits = List.filter (fun x -> x.owner <> gme.cTurn) exits in
  let rec attack_helper ext =
    match ext with
    | [] -> None
    | h::t -> if should_attack (
                (success_vs_failure (attack_loc.troops - 1) (h.troops)) +.
                (build_value h attack_loc gme)) then Some h else
              attack_helper t in
  attack_helper exits

(* Returns which a pair consisting of which building to attack from and which
 * building to attack if a probability of an attack is high enough. Otherwise
 * it returns None. *)
let rec should_a_building_attack gme ai_builds : (building * building) option =
  match ai_builds with
  | [] -> None
  | h::t -> match should_this_building_attack gme h with
            | None -> should_a_building_attack gme t
            | Some x -> Some (h,x)

(* Returns a tuple consisting of whether a player should attack and if so what
 * building to attack from, what building to attack, and how many troops to
 * attack with. *)
let attaaaaaack gme =
  let ai_buildings = List.filter (fun x -> x.owner = gme.cTurn) gme.buildings in
  match (should_a_building_attack gme ai_buildings) with
  | None -> (false,empty (empty_region ()) "" [],empty (empty_region ()) "" [],0)
  | Some (a,d) ->
      let () = Printf.printf "%s decided to attack %s from %s with %d troops \n"
      gme.cTurn d.name a.name (min (a.troops -1) 3) in
      (true,a,d, min (a.troops - 1) 3)




