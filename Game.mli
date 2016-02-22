open Buildings
open Player

(* Current phase of the game *)
type phase = Init of (string*int) list | Dist of int | Attack of bool | Fort

(* Game state *)
type game = {
  phase : phase;
  buildings : building list;
  players : player list;
  cTurn : string
}

(* Given a game state and two buildings, checks whether there is a path between
 * the two buildings entirely owned by the player who owns the buildings *)
val has_path : game -> building -> building -> bool

(* Finds the total number of troops a player owns *)
val get_player_troops : player -> game -> int