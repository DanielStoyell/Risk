(* Buildings, in the abstract, are the "countries" of the standard Risk game
that contain information about their properties as well as their borders.
Specifically, it is easiest for this code to think of buildings as nodes in a
network. *)

(* A region of the Cornell campus *)
type region = {
  name : string;
  buildings : string list;
  star_value : int
}

  (* The building itself - stores data about the building*)
  type building ={
  name : string;
  owner : string;
  troops : int;
  region : region;
  neighbors : string list
}

  (* Given the name of a building and a list of all buildings, returns the
   * building. *)
  val string_to_build : string -> building list -> building

  (* Given the name of a building and a list of all buildings, returns the
   * correct case sensitive name of the building *)
  val string_to_build_string : string -> building list -> string

  (* Changes the control of the building *)
  val take_over : building -> string -> building

  (* Creates an unoccupied region *)
  val empty : region -> string -> string list -> building

  (* Creates an unoccupied region *)
  val empty_region : unit -> region

  (* Changes the number of troops on the building - adds troops to the building
  if it is a positive int and removes troops from the building if it is a
  negative int *)
  val update_troops : building -> int -> building

  (*Returns a building list of string building names*)
  val strings_to_builds : string list -> building list ->
   building list -> building list

  (* Checks to see if the building is occuppied *)
  val is_occupied : building -> bool


