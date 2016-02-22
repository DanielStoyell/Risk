open Buildings
open Player

let compNum = Random.self_init (); ref (-1)
let cpuNames = ["Prof Clarkson"; "Chirag"; "Ahmed";
                "John Hopcroft"; "Mike George"]
let cpuNames = List.map (fun e -> (Random.bits (), e)) cpuNames
let cpuNames = List.sort (fun a b -> fst a - fst b) cpuNames
let cpuNames = List.map (fun e -> snd e) cpuNames

(* RISK ASCII art credit: http://patorjk.com/software/taag *)
let () =
  Printf.printf "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n%s\n\n"

"
RRRRRRRRRRRRRRRRR      iiii                     kkkkkkkk
R::::::::::::::::R    i::::i                    k::::::k
R::::::RRRRRR:::::R    iiii                     k::::::k
RR:::::R     R:::::R                            k::::::k
  R::::R     R:::::R iiiiiii      ssssssssss     k:::::k    kkkkkkk
  R::::R     R:::::R i:::::i    ss::::::::::s    k:::::k   k:::::k
  R::::RRRRRR:::::R   i::::i  ss:::::::::::::s   k:::::k  k:::::k
  R:::::::::::::RR    i::::i  s::::::ssss:::::s  k:::::k k:::::k
  R::::RRRRRR:::::R   i::::i   s:::::s  ssssss   k::::::k:::::k
  R::::R     R:::::R  i::::i     s::::::s        k:::::::::::k
  R::::R     R:::::R  i::::i        s::::::s     k:::::::::::k
  R::::R     R:::::R  i::::i  ssssss   s:::::s   k::::::k:::::k
RR:::::R     R:::::R i::::::i s:::::ssss::::::s k::::::k k:::::k
R::::::R     R:::::R i::::::i s::::::::::::::s  k::::::k  k:::::k
R::::::R     R:::::R i::::::i  s:::::::::::ss   k::::::k   k:::::k
RRRRRRRR     RRRRRRR iiiiiiii   sssssssssss     kkkkkkkk    kkkkkkk

CS 3110 Final Project | Fall 2015 | Page Bowers, James Cramer, Richa Deshpande, and Dan Stoyell

#####################################################################"

(* Prompts user(s) to enter the number of humans playing. Prompts them again
 * if the input provided is not a number or is not in our bounds of gameplay.*)
let rec getNumHumans () =
  let () = print_string "How many human players are playing?\n" in
  let command = read_line () in
  match command with
  | "0" | "1" | "2" | "3" | "4" | "5" | "6" -> int_of_string(command)
  | _ ->
    let () = print_string "Improper input! Be sure to enter a number
    1-6 inclusive in numeric form!\n" in
    getNumHumans ()


(* Prompts user(s) to enter the number of computers playing (AI's). Prompts them
 * again if the input provided is not a number or is not in our bounds of gameplay.*)
let rec getNumComps () =
  let () = print_string "How many computers are playing?\n" in
  let command = read_line () in
  match command with
  | "1" | "2" | "3" | "4" | "5" | "0" -> int_of_string(command)
  | _ ->
    let () = print_string "Improper input! Be sure to enter a number
    0-5 inclusive in numeric form!\n" in
    getNumComps ()

(*Prompts the user to name themselves.*)
let rec askName () =
  let () = print_string "What would you like to name this human?\n" in
  read_line ()

(* Initializes the game by gathering information on how many of each of humans
 * and AI's are playing. Prints appropriate error messages if some data is
 * incorrect and prompts user to re-enter. Initializes a random order for the
 * players to distibute troops at the start of the game. *)
let rec asker () =
  let shellPlayer = empty_player () in
  let numHumans = getNumHumans () in
  let numComps = getNumComps () in
  if numHumans + numComps < 2 then
  let () = Printf.printf "You need at least 2 players! Try again. \n" in
  asker () else
  if numHumans + numComps > 6 then
  let () = Printf.printf "You can only play with a maximum of six players! Try again. \n" in
  asker () else
  let rec initPlayers hums comps pList =
    if hums > 0 then
      let pName = askName () in
      initPlayers (hums-1) comps ({shellPlayer with name=pName}::pList)
    else if comps > 0 then
      let cName = compNum := !compNum + 1; List.nth cpuNames !compNum in
      let toAdd = {name=cName; buildings=[]; stars=0; ai=true} in
      (initPlayers hums (comps-1) (toAdd::pList))
    else pList in
  initPlayers numHumans numComps []

(* Creates a random turn order for the human and AI players. *)
let get_turn_order (pList : player list) =
  let tagged = List.map (fun elt -> (Random.bits (), elt)) pList in
  let sorted = List.sort (fun e1 e2 -> (fst e1) - (fst e2)) tagged in
  List.map (fun elt -> (snd elt).name) sorted

(* Initializes the amount of troops designated for each player based on how many
 * players are playing. *)
let get_init_pieces (pList : player list) =
  let start_pieces = 40 - 5*((List.length pList)-2) in
  List.map (fun e -> (e.name, start_pieces)) pList

(* Hardcodes the list of geogrphy of the game map. *)
let get_geography () =
  let collegetown =
  {
    name = "Collegetown";
    buildings = ["CTB"; "Starbucks"; "Dunbars"; "Level B"];
    star_value = 2
  } in
  let engineers =
  {
    name = "Engineering Quad";
    buildings = ["Sage Hall"; "Thurston Hall"; "Rhodes Hall"; "Statler Hall";
    "Gates Hall"; "Upson Hall"; "Olin Hall"; "Duffield"; "Hollister Hall";
    "Carpenter Hall"];
    star_value = 7
  } in
  let arts =
  {
    name = "Arts Quad";
    buildings = ["Willard Straight Hall"; "McGraw Clock Tower"; "Uris Library";
    "Olin Library"; "Goldwin-Smith Hall"; "Architecture School"];
    star_value = 3
  } in
  let west =
  {
    name = "West Campus";
    buildings = ["Cook House"; "Becker House"; "Bethe House"; "Rose House";
    "Keeton House"];
    star_value = 2
  } in
  let ag =
  {
    name = "Ag Quad";
    buildings = ["Bailey Hall"; "Mallott Hall"; "Kennedy Hall";
    "Plant Science Building"; "Mann Library"; "Warren Hall";
    "College of Human Ecology"];
    star_value = 3
  } in
  let north =
  {
    name = "North Campus";
    buildings = ["Balch Hall"; "CKB"; "Appel"; "Mews Hall"; "Donlon Hall";
    "RPCC"; "Highrises"; "Lowrises"; "Townhouses"];
    star_value = 5
  } in
  let ctb =
  {
    name = "CTB";
    owner = "";
    region = collegetown;
    troops = 0;
    neighbors = ["Starbucks"; "Hollister Hall"]
  } in
  let starbucks =
    {
      name = "Starbucks";
      owner = "";
      region = collegetown;
      troops = 0;
      neighbors = ["Dunbars"; "Level B"; "CTB"]
    } in
  let dunbars =
    {
      name = "Dunbars";
      owner = "";
      region = collegetown;
      troops = 0;
      neighbors = ["Level B"; "Starbucks"]
    } in
  let levelb =
    {
      name = "Level B";
      owner = "";
      region = collegetown;
      troops = 0;
      neighbors = ["Dunbars"; "Starbucks"]
    } in
  let sage =
    {
      name = "Sage Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Statler Hall"; "Olin Hall"; "Duffield"; "Olin Library"]
    } in
  let thurston =
    {
      name = "Thurston Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Hollister Hall"; "Carpenter Hall"; "Upson Hall"]
    } in
  let rhodes =
    {
      name = "Rhodes Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Gates Hall"; "Upson Hall"]
    } in
  let statler =
    {
      name = "Statler Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Mallott Hall"; "Gates Hall"; "Duffield"; "Sage Hall"]
    } in
  let gates =
    {
      name = "Gates Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Statler Hall"; "Rhodes Hall"; "Duffield"]
    } in
  let upson =
    {
      name = "Upson Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Duffield"; "Rhodes Hall"; "Thurston Hall"]
    } in
  let olinh =
    {
      name = "Olin Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Willard Straight Hall"; "Sage Hall"; "Carpenter Hall";
      "Keeton House"]
    } in
  let duffield =
    {
      name = "Duffield";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Sage Hall"; "Statler Hall"; "Gates Hall";
                   "Upson Hall"; "Carpenter Hall"]
    } in
  let hollister =
    {
      name = "Hollister Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Carpenter Hall"; "Thurston Hall"; "CTB"]
    } in
  let carpenter =
    {
      name = "Carpenter Hall";
      owner = "";
      region = engineers;
      troops = 0;
      neighbors = ["Olin Hall"; "Hollister Hall"; "Duffield"; "Thurston Hall"]
    } in
  let wsh =
    {
      name = "Willard Straight Hall";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Olin Hall"; "McGraw Clock Tower"]
    } in
  let clocks =
    {
      name = "McGraw Clock Tower";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Uris Library"; "Bethe House"; "Willard Straight Hall"]
    } in
  let urisl =
    {
      name = "Uris Library";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Olin Library"; "McGraw Clock Tower"; "Rose House"]
    } in
  let olinl =
    {
      name = "Olin Library";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Goldwin-Smith Hall"; "Uris Library"; "Sage Hall"]
    } in
  let gsh =
    {
      name = "Goldwin-Smith Hall";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Bailey Hall"; "Architecture School"; "Olin Library"]
    } in
  let archies =
    {
      name = "Architecture School";
      owner = "";
      region = arts;
      troops = 0;
      neighbors = ["Goldwin-Smith Hall"; "Cook House"; "Balch Hall"]
    } in
  let cook =
    {
      name = "Cook House";
      owner = "";
      region = west;
      troops = 0;
      neighbors = ["Becker House"; "Architecture School"; "RPCC"]
    } in
  let becker =
    {
      name = "Becker House";
      owner = "";
      region = west;
      troops = 0;
      neighbors = ["Cook House"; "Rose House"]
    } in
  let bethe =
    {
      name = "Bethe House";
      owner = "";
      region = west;
      troops = 0;
      neighbors = ["Keeton House"; "Rose House"; "McGraw Clock Tower"]
    } in
  let rose =
    {
      name = "Rose House";
      owner = "";
      region = west;
      troops = 0;
      neighbors = ["Becker House"; "Bethe House"; "Uris Library"]
    } in
  let keeton =
    {
      name = "Keeton House";
      owner = "";
      region = west;
      troops = 0;
      neighbors = ["Olin Hall"; "Bethe House"]
    } in
  let bailey =
    {
      name = "Bailey Hall";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Goldwin-Smith Hall"; "Mallott Hall"; "Warren Hall";
      "College of Human Ecology"]
    } in
  let mallott =
    {
      name = "Mallott Hall";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Statler Hall"; "Kennedy Hall"; "Bailey Hall"]
    } in
  let kennedy =
    {
      name = "Kennedy Hall";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Plant Science Building"; "Mallott Hall"]
    } in
  let plant =
    {
      name = "Plant Science Building";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Mann Library"; "Kennedy Hall"]
    } in
  let mann =
    {
      name = "Mann Library";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Warren Hall"; "Plant Science Building"]
    } in
  let warren =
    {
      name = "Warren Hall";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["College of Human Ecology"; "Mann Library"; "Bailey Hall"]
    } in
  let humec =
    {
      name = "College of Human Ecology";
      owner = "";
      region = ag;
      troops = 0;
      neighbors = ["Balch Hall"; "Bailey Hall"; "Warren Hall"]
    } in
  let balch =
    {
      name = "Balch Hall";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["CKB"; "Architecture School"; "College of Human Ecology"]
    } in
  let ckb =
    {
      name = "CKB";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Mews Hall"; "Donlon Hall"; "Balch Hall"]
    } in
  let appel =
    {
      name = "Appel";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Mews Hall"; "Lowrises"]
    } in
  let mews =
    {
      name = "Mews Hall";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Donlon Hall"; "CKB"; "Appel"]
    } in
  let donlon =
    {
      name = "Donlon Hall";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["RPCC"; "Mews Hall"; "CKB"]
    } in
  let rpcc =
    {
      name = "RPCC";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Donlon Hall"; "Townhouses"; "Highrises"; "Cook House"]
    } in
  let highrises =
    {
      name = "Highrises";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Townhouses"; "Lowrises"; "RPCC"]
    } in
  let lowrises =
    {
      name = "Lowrises";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["Appel"; "Townhouses"; "Highrises"]
    } in
  let townhouses =
    {
      name = "Townhouses";
      owner = "";
      region = north;
      troops = 0;
      neighbors = ["RPCC"; "Highrises"; "Lowrises"]
    } in
  ([townhouses; lowrises; highrises; rpcc; donlon; mews; appel; ckb; balch;
  humec; warren; mann; plant; kennedy; mallott; bailey; keeton; rose; cook;
  bethe; becker; archies; gsh; olinl; urisl; clocks; wsh; carpenter; hollister;
  duffield; olinh; upson; gates; statler; rhodes; thurston; sage; levelb;
  dunbars; starbucks; ctb],[north; ag; west; arts; engineers; collegetown])