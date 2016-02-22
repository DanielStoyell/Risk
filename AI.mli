open Player
open Buildings
open Game

(* Where to place troops at beginning of game *)
val where_place_troops_begin : building list -> string -> game -> string

(* takes in number of troops attacks and num troops defending and returns
 * a probability in the form of a float *)
val success_vs_failure: int->int->float

(* Calculates how many troops are expected to be lost in an attack *)
val how_many_troops_lost : int -> int -> int -> float

(* Returns the enemy buildings of a given building *)
val building_neighbors: building -> building list -> building list

(*Returns a list of neighbors that are not major threats given the current
 * building and current building list*)
val friendly_building_neighbors: building -> building list -> building list

(* Return which building the AI should place troops on at beginning of their
 * turn*)
val where_to_place_troops: building list -> building list -> building

(* given a probability, will give whether or not to attack *)
val should_attack: float->bool

(* If troops are below a critical level, turn in stars.  If true, return in
 * stars, if false save your stars*)
val stars_trade_for_troops: string -> player list -> building list -> int

(* After capture a building, how many troops to move to new building *)
val troops_to_move : game -> building -> building -> int -> int

(* Int passed in is how many troops to distribute. It returns a building name
 * and how many troops should be placed on that building. *)
val to_distribute : game -> int -> int * string

(* Which building to place a single troop on*)
val aiInit : game -> string

(* Whether wants to fortify, building moving from, building moving to,
 * how many troops *)
val fortify_stuff : game -> bool * building * building * int


(* Whether wants to attack, building attacking from, building attacking,
 * how many troops attacking with*)
val attaaaaaack : game -> bool * building * building * int
