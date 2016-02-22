type player = {
  name : string;
  stars : int;
  buildings : string list;
  ai : bool
}

(* A placeholder or starter player filled with nothing *)
let empty_player () = {
  name = "";
  stars=0;
  buildings=[];
  ai = false
}

(* Takes the name of a player and returns the player associated with it *)
let string_to_player (strin:string) (playas:player list) : player =
  let new_players = List.filter (fun x -> x.name = strin) playas in
  match new_players with
  | [] -> failwith "We will already make sure it is a valid player"
  | h::_ -> h

(* Updates the stars on a player *)
let update_stars p i =
  let new_stars = p.stars + i in
  {p with stars = new_stars}

(* Changes the buildings a player owns, whether new or lost *)
let update_buildings p newb lostb =
  let b' = List.filter (fun b -> not (List.mem b lostb)) p.buildings in
  {p with buildings = b'@ newb}
