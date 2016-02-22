
(* Record representing region.*)
type region = {
  name : string;
  buildings : string list;
  star_value : int
}

(* Record representing building.*)
type building = {
  name : string;
  owner : string;
  troops : int;
  region : region;
  neighbors : string list
}

(* Creates an empy region record with all fields set to empty. *)
let empty_region () =
  {
    name = "";
    buildings = [];
    star_value = 0
  }

(* Creates a building record with all fields set to the entered fields or empty.*)
let empty reg nme bors =
  {
    name = nme;
    owner = "";
    troops = 0;
    region = reg;
    neighbors = bors
  }

(* Given a string, outputs the building record with that name. *)
let string_to_build (strin:string) (build_lst : building list) : building =
  let new_builds = List.filter (fun x -> x.name = strin) build_lst in
  match new_builds with
  | [] -> failwith (strin ^ " is not a valid building name")
  | h::_ -> h

(* Given a builidng name checks if the building name is actually valid and
 * returns the building name we have hardcoded so that everything is standard.*)
let string_to_build_string (strin:string) (build_lst : building list) : string =
  let new_builds =
  List.filter (fun x -> String.lowercase x.name = String.lowercase strin)
    build_lst in
  match new_builds with
  | [] -> failwith (strin ^ " is not a valid building name")
  | h::_ -> h.name

(* Creates a building list given a string list of the corresponding names. *)
let rec strings_to_builds build_strings buildings acc =
    match build_strings with
    | [] -> acc
    | h::t -> let new_h = string_to_build h buildings in
              strings_to_builds t buildings (new_h::acc)

(* Returns a new building record with the owner updated to the player that has
 * taken over this building.*)
let take_over b new_owner =
  {b with owner = new_owner}

(* Returns a new building record with the troops updated so amount of troops
 * gained or lost is added to the old troops.*)
let update_troops b new_troops =
  {b with troops = b.troops + new_troops}

(* Returns a boolean describing if a building is owned or not.*)
let is_occupied b =
  b.owner <> ""

