open Buildings
open Player

(*
 * Gets all the necessary info to start the game.
 * - Asks how many humans are playing and their names
 * - Asks how many CPUs are playing
 * - Main function spits out
 *     - Start buildings (unoccupied building list)
 *     - Start players (player list)
 *     - Turn order list
 *)

(* Asks how many humans and computers are playing and returns a list of their
 * names *)
val asker : unit -> player list

(* Given a list of players returns a list of those player names in a randomized
 * order *)
val get_turn_order : player list -> string list

(* Returns the game board with a list of the buildings in the game and a list
 * of the regions in the game *)
val get_geography : unit -> (building list * region list)

(* Given a list of players, returns a list of player names tupled with how many
 * troops they have at the start of the game *)
val get_init_pieces : player list -> (string*int) list