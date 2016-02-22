open Buildings
open Player
open Setup
open Gui
open AI
open Game

let start_players = asker ()
let geo = get_geography ()
let start_buildings = fst geo
let regions = snd geo
let turn_order = ref(get_turn_order start_players)

(* String comparison function.
 * Compares both strings after converting them to lowercase.
 * Returns 0 if equal and -1 or 1 is not equal. *)
let str_comp s1 s2 =
  let s1' = (Bytes.lowercase s1) in
  let s2' = (Bytes.lowercase s2) in
  compare s1' s2'

(* Given a building from, a building to, and a number to move,
 * returns a building list with the updated troop numbers *)
let fortify game (b1:building) (b2:building) n =
  let replace = (fun (b:building) ->
    if b.name = b1.name then {b with troops = b.troops - n}
    else if b.name = b2.name then {b with troops = b.troops + n} else b) in
  List.map replace game.buildings

(* Given a player name and a player list, tells whether the string is an AI *)
let isAI str plyrs = (string_to_player str plyrs).ai

(* After a building has been taken over, asks for the number of troops
 * to move, makes sure it is valid, and returns an updated building list *)
let rec troops_in_takeover a_troops (new_building : building)
  (old_building : building) game_log =
  let rec find_troops () =
    let comand = read_line () in
    let troop = try int_of_string comand with
      | _ -> let () = Printf.printf "Try again with an integer input!\n" in
      find_troops () in
    if troop < a_troops then
      let () =
      (Printf.printf "You have to move at least %d troops over. Try again!\n"
      a_troops) in
      find_troops () else
    let o_building = List.find (fun (x : building) -> x.name = old_building.name)
      game_log.buildings in
    if troop >= o_building.troops then
      let () =
      Printf.printf "You don't have that many troops to move. Try again!\n" in
      find_troops () else
    troop in
  let rec total_troops_moved () =
    let troop = if isAI game_log.cTurn game_log.players
       then troops_to_move game_log old_building new_building a_troops else
    find_troops () in
    let n_building =
    {name = new_building.name; owner = game_log.cTurn; troops = troop;
      region = new_building.region; neighbors = new_building.neighbors} in
    let o_building = update_troops old_building (0 - troop) in
    List.map (fun (b:building) -> if (b.name = new_building.name) then
         n_building else if (b.name = old_building.name) then o_building else b)
         game_log.buildings in
  if isAI game_log.cTurn game_log.players then total_troops_moved () else
  let () =
  Printf.printf "Congratulations, you took over the building! How many troops would you like to move over to the new building? \n" in
  total_troops_moved ()

(* Simulates a dice roll as in Risk, and returns a tuple of the troops each
 * building lost *)
let dice_roll attack_troop defense_troop =
  let ind_dice_roll () = (Random.int 5) + 1 in
  let rec det_troops troops_to_lose a_rolls d_rolls atroops dtroops =
    if troops_to_lose = 0 then (atroops, dtroops) else
    let aroll =
      match a_rolls with
      | [] -> failwith "Impossible"
      | h::t -> (h,t)
    in
    let droll =
      match d_rolls with
      | [] -> failwith "Impossible"
      | h::t -> (h,t)
    in
    if fst(aroll) > fst(droll) then
      det_troops (troops_to_lose-1) (snd(aroll)) (snd(droll)) atroops (dtroops+1)
    else
      det_troops (troops_to_lose-1) (snd(aroll)) (snd(droll)) (atroops+1)
      dtroops in
  let attack_rolls =
    match attack_troop with
    | 1 -> [ind_dice_roll ()]
    | 2 -> let dice_roll_1 = ind_dice_roll () in
           let dice_roll_2 = ind_dice_roll () in
           if  dice_roll_1 < dice_roll_2 then [dice_roll_2; dice_roll_1] else
           [dice_roll_1; dice_roll_2]
    | 3 -> let dice_roll_1 = ind_dice_roll () in
           let dice_roll_2 = ind_dice_roll () in
           let dice_roll_3 = ind_dice_roll () in
           if dice_roll_1 <= dice_roll_2 && dice_roll_1 <= dice_roll_3 then
             if dice_roll_2 < dice_roll_3 then
             [dice_roll_3; dice_roll_2; dice_roll_1] else
             [dice_roll_2; dice_roll_3; dice_roll_1]
           else if dice_roll_1 <= dice_roll_2 && dice_roll_3 <= dice_roll_1 then
             [dice_roll_2; dice_roll_1; dice_roll_3]
           else if dice_roll_1 >= dice_roll_2 && dice_roll_3 >= dice_roll_1 then
             [dice_roll_3; dice_roll_1; dice_roll_2]
           else if dice_roll_2 >= dice_roll_3 then
             [dice_roll_1; dice_roll_2; dice_roll_3]
           else [dice_roll_1; dice_roll_3; dice_roll_2]
    | _ -> failwith "Impossible" in
  let defense_rolls =
    match defense_troop with
    | 1 -> [ind_dice_roll ()]
    | 2 -> let ddice_roll_1 = ind_dice_roll () in
           let ddice_roll_2 = ind_dice_roll () in
           if ddice_roll_1 < ddice_roll_2 then [ddice_roll_2; ddice_roll_1] else
           [ddice_roll_1; ddice_roll_2]
    | _ -> failwith "Impossible" in
  det_troops (min attack_troop defense_troop) attack_rolls defense_rolls 0 0

(* Adds the star bonus to the troops passed in *)
let troops_after_stars troops_wo_stars stars =
  match stars with
  | 0 -> troops_wo_stars
  | 2 -> 2 + troops_wo_stars
  | 3 -> 4 + troops_wo_stars
  | 4 -> 7 + troops_wo_stars
  | 5 -> 10 + troops_wo_stars
  | 6 -> 13 + troops_wo_stars
  | 7 -> 17 + troops_wo_stars
  | 8 -> 21 + troops_wo_stars
  | 9 -> 25 + troops_wo_stars
  | 10 -> 30 + troops_wo_stars
  | _ -> failwith "Impossible, should already check stars between 2 & 10"

(* Returns whether the player owns all the buildings in the region *)
let hasRegion (p : player) (r : region) : bool =
  let owns b = List.exists (fun n -> b = n) p.buildings in
  List.fold_left (fun a b -> a && owns b) true r.buildings

(* Asks for the number of stars to trade in, validates it, and returns *)
let rec getStars p =
  let () =
  Printf.printf "Calculating troop bonus for %s. Enter a number of your %d stars to trade:\n"
    p.name p.stars in
  let command = read_line () in
  let sNum = try int_of_string command with
  | _ -> let () = Printf.printf "Try again with integer input!\n" in
    getStars p in
  if (sNum < 0) then
    let () = Printf.printf "You have entered an invalid number of stars. Try again!\n" in
    getStars p
  else if (sNum = 1) then
    let () = print_endline "You cannot only trade in one star! Try again.\n" in
    getStars p
  else if (sNum > 10) then
    let () = print_endline "You can only trade in at most 10 stars! Try again.\n" in
    getStars p
  else if (sNum > p.stars) then
    let () = print_endline "You don't have that many stars! Try again.\n" in
    getStars p
  else sNum

(* Calculates the number of new troops recieved at the beginning of a turn *)
let num_new_troops p game =
  let star_bonus =
  if p.ai then stars_trade_for_troops p.name game.players game.buildings
  else getStars p in
  let start_amount = 3 in
  let building_bonus = (List.length p.buildings)/3 in
  let region_bonus =
  List.fold_left (fun a r -> if hasRegion p r then a + r.star_value else a)
      0 regions in
  let without_stars = start_amount + building_bonus + region_bonus in
  (troops_after_stars without_stars star_bonus, star_bonus)

(* Finds the next player in the turn order *)
let next_player name : string =
  let rec get_next order =
    match order with
    | [] -> List.hd !turn_order
    | [x] -> List.hd !turn_order
    | h::x::t -> if h = name then x else get_next (x::t) in
        get_next !turn_order

(* Driver for the initialization stage of the game *)
let rec do_init (game : game) (lst : (string*int) list) : game =
  if List.for_all (fun (s,n) -> n = 0) lst then
  (let first_player =
  List.find (fun e -> e.name = (List.hd !turn_order)) game.players in
  let () =
    (Printf.printf "Initialize troop placement finished! Time to start the game.\n") in
  let (new_troops, stars_traded) = num_new_troops first_player game in
  let newPlayers = List.map (fun e -> if e.name = first_player.name
      then {e with stars = e.stars - stars_traded} else e) game.players in
  {
    phase = Dist new_troops;
    buildings = game.buildings;
    players = newPlayers;
    cTurn = List.hd !turn_order
  })
  else
    let command = if isAI game.cTurn game.players then aiInit game else
      let () =
        Printf.printf "It is %s's turn to place troops, with %d troops remaining. Enter a building:\n"
        game.cTurn (List.assoc game.cTurn lst) in
      read_line () in
    let isValid =
    List.exists (fun (e : building) -> ((str_comp e.name command) = 0) &&
      ((List.for_all (fun a-> is_occupied a) game.buildings
       && e.owner = game.cTurn) ||
      (not (is_occupied (e))))) game.buildings in
    if isValid then
      let command = string_to_build_string command game.buildings in
      let newPhase = (Init (List.map (fun (s, n) -> if s = game.cTurn then
            (s, (n-1)) else (s,n)) lst)) in
      let newBuildings = (List.map (fun (e : building) ->
        if ((str_comp e.name command) = 0)
        then update_troops (take_over e game.cTurn) 1 else e) game.buildings) in
      let newPlayers = List.map (fun p -> if p.name = game.cTurn then
            if List.mem command p.buildings then p else
            {p with buildings = (command)::p.buildings} else p) game.players in
      let newTurn = (next_player game.cTurn) in
      {
        phase = newPhase;
        buildings = newBuildings;
        players = newPlayers;
        cTurn = newTurn;
      }
    else
      let isValidName = List.exists (fun (e : building) ->
          ((str_comp e.name command) = 0)) game.buildings in
      if not isValidName then
        let () = print_endline "That isn't a valid building name! Try again." in
        do_init game lst
      else
      let areAllOccupied =
        List.for_all (fun a -> is_occupied a) game.buildings in
      if areAllOccupied then
        let () = print_endline "You cannot place a troop on someone else's building! Try again." in
        do_init game lst
      else
        let () =
        Printf.printf "You must place a troop on an unoccupied building! Try again.\n" in
        do_init game lst

(* Gets a building input from the user and validates it *)
let rec get_building game n =
   let () = Printf.printf "It is %s's turn.\n" game.cTurn in
   let () =
      Printf.printf "You have %d troops. Pick a building to place troops on.\n"
      n in
   let build = read_line () in
  let isValidBuild = List.exists (fun (e : building) ->
    ((str_comp e.name build)=0) &&
    (e.owner = game.cTurn)) game.buildings in
  let isYourBuild = List.exists (fun (e : building) ->
    ((str_comp e.name build)=0) &&
    (e.owner = game.cTurn)) game.buildings in
  if not isValidBuild then let _ = print_endline "That isn't a valid building!
  Try again.\n" in (get_building game n) else
  if not isYourBuild then let _ = print_endline "You don't own that building! Try again.\n" in
  (get_building game n) else
  build

(* Gets a placement number input from the user and validates it *)
let rec get_tnum n  =
  let () = print_endline "How many troops do you want to place?" in
  let tnum = read_line () in
  let tnum =
    try
      int_of_string tnum
    with
      | _ -> print_endline "Invalid number of troops! Please use an integer.";
      get_tnum n
    in
    if (tnum <= n) && (tnum > 0) then tnum
    else let _ =
      print_endline "Invalid number of troops! Try again.\n" in
    (get_tnum n)

(* Driver for the distribution phase of a turn *)
let rec do_dist game n =
  let aiTurn = isAI game.cTurn game.players in
  let (tnum, build) = if aiTurn then to_distribute game n else
  (get_tnum n, get_building game n) in
  if n-tnum > 0 then
  {
   phase = Dist (n-tnum);
   buildings =
      (List.map (fun (b:building) -> if ((str_comp b.name build)=0) then
         update_troops b tnum else b)
        game.buildings);
   players = game.players;
   cTurn = game.cTurn
  }
  else
  {
   phase = Attack false;
   buildings =
      (List.map (fun (b:building) -> if ((str_comp b.name build)=0) then
         update_troops b tnum else b)
        game.buildings);
   players = game.players;
   cTurn = game.cTurn
  }

(* Asks the player if they wish to attack *)
let rec cont game =
  let () = Printf.printf "%s, would you like to attack? (y/n)\n" game.cTurn in
  let command = read_line () in
  match (Bytes.lowercase command) with
  | "y" ->
    let builds = List.map (fun n -> string_to_build n game.buildings)
        (string_to_player game.cTurn game.players).buildings in
    let more_one = List.filter (fun b -> b.troops > 1) builds in
    let is_neighbor_friend = fun n -> (string_to_build n game.buildings).owner =
        game.cTurn in
    let has_neighbors =
    List.filter (fun b -> (List.filter (fun n -> not (is_neighbor_friend n))
    b.neighbors) <> []) more_one in
    if has_neighbors <> [] then true else
    let () =
    Printf.printf "There are no valid buildings to attack! Skipping this phase.\n" in
    false
  | "n" -> false
  | _ -> let () = Printf.printf "Invalid choice! Try again.\n" in cont game

(* Finds the attacking and defending building from user *)
let get_attack game =
  let rec get_attacker () =
    let rec get_attack_building () =
      let () = Printf.printf "Choose a building to attack from:\n" in
      let command = read_line () in
      let rec find_attack_building comd (buildings:building list) =
        match buildings with
        | [] -> let () =
           Printf.printf "Invalid building name! Try again.\n" in
           get_attack_building ()
        | h::t -> if ((str_comp comd h.name)=0) && h.owner = game.cTurn
                then (h,h)
                else if ((str_comp comd h.name)=0) then
                let () = print_endline "You don't own that building! \n" in
                get_attack_building ()
                else find_attack_building comd t in
      find_attack_building command game.buildings in
    let att = fst(get_attack_building ()) in
    if att.troops < 2 then let () =
    Printf.printf "You don't have enough troops on that building to attack with! Try again. \n" in
    get_attacker () else
    let () = Printf.printf "Choose a building to attack:\n" in
    let command = read_line () in
    let rec helper cmd (build:building) (buildings:building list) =
      match buildings with
      | [] -> (att,att)
      | h::t -> if ((str_comp h.name cmd)=0) && ((str_comp h.owner game.cTurn)<>0)
      && (List.exists (fun n -> n=build.name) h.neighbors) then (h,h) else
        if h.name = cmd then
          if h.owner = game.cTurn then (att,h) else
          (h,att) else
        helper cmd build t in
    let def = helper command att game.buildings in
    if def = (att,att) then
    let () =
      Printf.printf "Make sure you don't own this building, there is a connection to this building, or your spelling! Try again.\n"
    in get_attacker () else
    if fst(def) = att then
    let () = print_endline "You can't attack your own building! Try again.\n" in
    get_attacker () else
    if snd(def) = att then
    let () =
      print_endline "That building doesn't border the building you are attacking from! Try again.\n" in
    get_attacker () else
    (att,fst(def)) in
  get_attacker ()

(* Eliminates players, or ends the game if only one player left *)
let is_player_eliminated players (attack:string)
    (defense : string) : player list =
  let rec find_player n playa =
    match playa with
    | [] -> failwith "Should never fail to find a valid player"
    | h::t -> if h.name = n then h else find_player n t in
  let attacker = find_player attack players in
  let defender = find_player defense players in
  let player_list = List.filter (fun (x:player) ->
    match x.buildings with
    |[] -> false
    |_::_ -> true) players in
  if players = player_list then players else
  let attack_stars = attacker.stars + defender.stars in
  let () = turn_order := List.filter (fun x -> x <> defense) !turn_order in
  let new_players = List.map (fun (x:player) -> if x.name = attack then
    {x with stars = attack_stars} else x) player_list in
  match new_players with
  | [] -> failwith "Impossible - must have one player"
  | h::[] -> let () = Printf.printf "Congratulations %s, you won! \n" h.name in
             exit 0
  | _ -> new_players

(* Given an attacker, defender, and number of troops to attack with,
 * returns a new game state after the attack *)
let attack game a d n =
  let attacker = a.owner in
  let defender = d.owner in
  let lost = dice_roll n (min d.troops 2) in
  let () = Printf.printf "%s lost %d troops\n" a.name (fst lost) in
  let () = Printf.printf "%s lost %d troops\n" d.name (snd lost) in
  let a = {a with troops = a.troops - (fst lost)} in
  let d = {d with troops = d.troops - (snd lost)} in
  let newBuildings = List.map (fun (b:building) ->
    if b.name = a.name then a
    else if b.name = d.name then d else b) game.buildings in
  if d.troops <> 0 then
    (newBuildings, game.players, false)
  else
    let updated_game =
      {
        phase = game.phase;
        buildings = newBuildings;
        players = game.players;
        cTurn = game.cTurn;
      } in
    let finalBuildings = troops_in_takeover n d a updated_game in
    let finalPlayers = (List.map (fun p -> if p.name = attacker then
     {p with buildings = (d.name)::(p.buildings)} else if p.name = defender then
     {p with buildings =
        (List.filter (fun b -> b<>d.name) p.buildings)} else p) game.players) in
    let newfinalPlayers = is_player_eliminated finalPlayers attacker defender in
    (finalBuildings, newfinalPlayers, true)

(* Finds the attacker from user *)
let rec get_attackers att gme=
  let () = Printf.printf "Number of troops to attack with: " in
  let n = read_line () in
  let n = try int_of_string n with
  | _ -> let () = Printf.printf "Not an integer! Try again.\n" in
    get_attackers att gme in
  if n > 3 then
    let () = Printf.printf "You can't attack with more than three troops at once. Try again!\n" in
    get_attackers att gme
  else if n = (att.troops) then
    let () =
    Printf.printf "You don't have enough troops to sustain this building if you win! Choose at most 1 less troops.\n" in
    get_attackers att gme
  else if n<1 then
    let () = Printf.printf "You must move at least one troop. Try again!\n" in
    get_attackers att gme
  else n

(* Driver for the attack phase of a turn *)
let do_attack game =
  let aiTurn = isAI game.cTurn game.players in
  let (c, atta, defe, num) = if aiTurn then attaaaaaack game
  else (true, List.hd game.buildings, List.hd game.buildings, 1) in
  if (if aiTurn then not c else not (cont game)) then
    match game.phase with
    | Attack true -> let newStars = if (Random.float 1.) < 0.25 then 2 else 1 in
    let () =
      Printf.printf "%s recieved %d star(s) for capturing a building this turn!\n"
        game.cTurn newStars in
      {
        phase = Fort;
        buildings = game.buildings;
        players =
          List.map
            (fun p -> if p.name = game.cTurn then {p with stars =
                  p.stars + newStars} else p) game.players;
        cTurn = game.cTurn
      }
    | Attack false ->
      {game with phase = Fort}
    | _ -> failwith "Impossible" else
  let (att, def) = if aiTurn then (atta,defe) else get_attack game in
  let numTroops = if aiTurn then num else get_attackers att game in
  let (newBuildings, newPlayers, taken) = attack game att def numTroops in
  let has_taken = taken || match game.phase with
  | Attack true -> true
  | Attack false -> false
  | _ -> false in
  {
    phase = Attack has_taken;
    buildings = newBuildings;
    players = newPlayers;
    cTurn = game.cTurn;
  }

(* Finds if player wants to fortify *)
let rec want_fort game =
  let () = Printf.printf "Fortify? (y/n): " in
  let a = read_line () in
  match (Bytes.lowercase a) with
  | "y" ->
    let builds = List.map (fun n -> string_to_build n game.buildings)
      (string_to_player game.cTurn game.players).buildings in
    let more_one = List.filter (fun b -> b.troops > 1) builds in
    let is_neighbor_friend =
      fun n -> (string_to_build n game.buildings).owner = game.cTurn in
    let has_neighbors =
      List.filter (fun b -> (List.filter is_neighbor_friend b.neighbors) <> [])
      more_one in
    if has_neighbors <> [] then true else
    let () =
    Printf.printf "There are no valid buildings to fortify! Skipping this phase.\n" in
    false
  | "n" -> false
  | _ -> let () = Printf.printf "Invalid input! Try again.\n" in
    want_fort game

(* Gets what building player wants to go to *)
let rec get_b_to game bFrom =
  let () = Printf.printf "Buildings to move troops to : " in
  let b = read_line () in
  let b =
  try
  List.find (fun (e:building) -> ((str_comp e.name b) = 0)) game.buildings with
  | _ -> let () = Printf.printf "Building does not exist. Try again!\n" in
    get_b_to game bFrom in
  if ((str_comp game.cTurn b.owner) <> 0) then
    let () = Printf.printf "You do not own that building. Try again!\n" in
    get_b_to game bFrom
  else if not (has_path game bFrom b) then
    let () =
      Printf.printf "There is not path between those buildings. Try again!\n" in
    get_b_to game bFrom
  else b

(* Finds what building player want to move troops from *)
let rec get_b_from game =
  let () = Printf.printf "Buildings to move troops from: " in
  let b = read_line () in
  let b =
  try
  List.find (fun (e:building) -> ((str_comp e.name b)=0)) game.buildings with
  | _ -> let () = Printf.printf "Building does not exist. Try again!\n" in
    get_b_from game in
  if ((str_comp game.cTurn b.owner) <> 0) then
  let () = Printf.printf "You do not own that building. Try again!\n" in
  get_b_from game else
  if b.troops < 2 then let () =
    Printf.printf "You do not have enough troops on that building to fortify. Try again!\n" in
    get_b_from game else
  let good_neighbors = friendly_building_neighbors b game.buildings in
  if good_neighbors = [] then
  let () =
  Printf.printf "This building does not have any neighbors you own so you cannot fortify from it. Try again!\n" in get_b_from game
  else b

(* Finds number of troops player wishes to move *)
let rec get_to_move bFrom =
  let () = Printf.printf "Number of troops to move: " in
  let n = read_line () in
  let n = try int_of_string n with
  | _ -> let () = Printf.printf "Not an integer! Try again.\n" in
    get_to_move bFrom in
  if n > (bFrom.troops-1) then let () =
    Printf.printf "Not enough troops available to move. Try again!\n" in
    get_to_move bFrom
  else if n<1 then let () =
    Printf.printf "You must move at least one troop. Try again!\n" in
    get_to_move bFrom
  else n

(* Driver for the fortification phase of a turn *)
let do_fort game =
  let () = Printf.printf "FORTIFICATION:\n" in
  let aiTurn = isAI game.cTurn game.players in
  let (want, bF, bT, tM) = if aiTurn then fortify_stuff game
  else (true, List.hd game.buildings, List.hd game.buildings, 1) in
  if not (if aiTurn then want else (want_fort game)) then
    let (new_troops, stars_traded) = num_new_troops
        (List.find (fun p -> p.name = (next_player game.cTurn)) game.players)
        game in
    let newPlayers = List.map (fun e -> if e.name = (next_player game.cTurn)
        then {e with stars = e.stars - stars_traded} else e) game.players in
    {
      phase = Dist new_troops;
      buildings = game.buildings;
      players = newPlayers;
      cTurn = next_player game.cTurn
    }
  else
  let bFrom = if aiTurn then bF else get_b_from game in
  let bTo = if aiTurn then bT else get_b_to game bFrom in
  let toMove = if aiTurn then tM else get_to_move bFrom in
  let newBuildings = fortify game bFrom bTo toMove in
  let next_turn = next_player game.cTurn in
  let (new_troops, stars_traded) = num_new_troops
    (List.find (fun p -> p.name = next_turn) game.players) game in
  let newPlayers = List.map (fun e -> if e.name = next_turn
      then {e with stars = e.stars - stars_traded} else e) game.players in
  {
    phase = Dist new_troops;
    buildings = newBuildings;
    players = newPlayers;
    cTurn = next_turn
  }

(* Output system for the game. Prints console output and writes game data
 * to an HTML file that refreshes itself. *)
let gui (game : game) : unit =
  let _ = read_line () in
  let player_print p =
    let () = if p.name = game.cTurn then Printf.printf ">>> " else () in
    let () = Printf.printf "%s: %d stars, %d buildings\n"
         p.name p.stars (List.length p.buildings) in
    let () = Printf.printf "Regions: " in
    let _ = List.fold_left (fun a b -> if hasRegion p b then
      if a then (Printf.printf "%s" b.name; false)
      else (Printf.printf ", %s" b.name; false) else a) true regions in
    let () = Printf.printf "\nBuildings: " in
    let to_print = List.sort compare p.buildings in
    let _ = List.fold_left (fun a b ->
      if a then (Printf.printf "%s" b; false)
      else (Printf.printf ", %s" b; false)) true to_print in
    Printf.printf "\n-----------------------------\n" in
  let () = Printf.printf "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
                          \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" in
  let _ = List.map player_print game.players in
  let _ = Printf.printf "\n\n\n" in
  let _ = write_html_file game.buildings game.players in
  let _ = write_css_file game.buildings game.players in ()

(* Driver for the entire game *)
let rec engine game : unit =
  let () = gui game in
  match game.phase with
  | Init lst -> engine (do_init game lst )
  | Dist n -> engine (do_dist game n)
  | Attack _ -> engine (do_attack game)
  | Fort -> engine (do_fort game)

let init_list = get_init_pieces start_players in
let start_game =
  {
   phase = Init (init_list);
   buildings = start_buildings;
   players = start_players;
   cTurn = List.hd !turn_order
  } in
engine start_game
