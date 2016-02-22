open Buildings
open Player
open Setup
open Test_info
open AI
open Game

(* Gameplay handles the flow of gameplay *)

(* ####################################
        General values / functions
   #################################### *)


(* ####################################
        Distribution functions
   #################################### *)

(* Calculates how many new troops a player gets at the start of a turn,
returning that number of troops and the number of stars the player
decided to trade. *)
val num_new_troops : player -> game -> (int * int)

(* Takes in the number of new troops received without stars and the number of
stars and returns a new number of troops received for distribution *)
val troops_after_stars : int -> int -> int


(* ###################################
          Attack functions
   ################################### *)

(* Given the number of troops that each player is using in an attack, return
how many troops each player loses *)
val dice_roll : int -> int -> int * int

(*Given two buildings and the number of troops attacking, returns
the updated building and player lists*)
val attack : game -> building -> building -> int ->
  (building list) * (player list) * bool

(* If you take over the building, given how many troops you attacked with,
  you must decide how many troops you want to move to the new building and
  return a building list *)
val troops_in_takeover : int -> building -> building -> game -> building list

(* ###################################
          Fortification functions
   ################################### *)

(* Given a building where troops are moving off of, a building where troops are
moving to, and the number of troops being moved, return the new building list *)
val fortify : game -> building -> building -> int -> building list


(* The main engine of the game, recursive *)
val engine : game -> unit