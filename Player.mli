(*
 * Player Module: Represents a player and its attributes. All players should be
 * modeled off this module.
 *)

type player = {name : string; stars:int; buildings: string list; ai: bool}

(*
 * [update_stars s add] updates the stars attribute of a player with an addition
 * of [add] to [s].
 *
 * The [s] is the current stars value of the player. The value [add] can be
 * negative, positive, or zero, depending on the obtained
 * card or lack of card or loss of card.
*)
val update_stars : player -> int -> player

(* Given the string name of a player and the list of players in the game returns
 * that player *)
val string_to_player : string -> player list -> player

(* Returns a human player with no buildings, stars, or name *)
val empty_player : unit -> player

(*
 * [update_buildings blst new_b lost_b] updates the buildings atribute of a
 * player appending [new_b] to [blst] and removing elements of [lost_b].
 *
 * The [blst] is the current buildings list of the player. The value of [new_b]
 * can be a list of buildings or empty, depending on what buildings, if any were
 * obtained during gameplay. The value of [lost_b] can be be a list of buildings
 * or empty, depending on what buildings, if any were lost during gameplay.
 *)
val update_buildings :
   player-> string list -> string list -> player

