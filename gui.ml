open Jg_types
open Buildings
open Player


let building_names = ["Townhouse";"Lowrises";"HighRise";"RPCC";"Donlon";"Mews";
"Appel";"CKB";"Balch";"HumanEcology";"Warren";"Mann";"PlantSciences"; "Kennedy";
"Mallott";"Bailey";"Keeton";"Rose"; "Cook"; "Bethe"; "Becker"; "Architecture";
"GoldwinSmith";"OlinLibrary"; "UrisLibrary"; "McGrawClocktower";
"WilliardStraight"; "Carpenter";"Hollister";"Duffield"; "OlinHall"; "Upson";
"Gates"; "Statler"; "Rhodes"; "Thurston";"Sage"; "LevelB"; "Dunbars";
"Starbucks"; "CTB"]

let make_tuple (name:string) (b:building) =
  (name, Tint b.troops)

let make_tuple_players (name:string) (b:building) =
  (name, Tint b.troops)

(*Pre: Building list and
*these names are in same order and lists are same length*)
let listofTuples prop=
List.map2 (fun a b->  make_tuple a b) building_names prop

let colorlist = ["#99ff66";"#ff6666";"#9900ff"; "#0066ff";"#ff66cc";
                "#8fbcdb"; "#00ff00";"#D4AF37"; "840E47"; "#6b6a64a"]
let rec assignColor colors players name =
  match (colors,players) with
  | (_,[]) -> "Black"
  | (h1::t1,h2::t2) -> if name =h2.name then h1 else assignColor t1 t2 name
  | _ -> failwith "should not get here in assignColor"

let listOfColorTuples prop players=
  List.map2 (fun a b -> (a, Tstr (assignColor colorlist players b.owner)))
  building_names prop

<<<<<<< HEAD
let make_html build =
  Jg_template.from_file "mappage.tmpl" ~models: (listofTuples build)
let make_css build players = Jg_template.from_file "style.tmpl"
    ~models: (listOfColorTuples build players)


let write_html_file build =
  Core.Std.Out_channel.write_all "game.html" ~data:(make_html build)
=======
let make_player_list players =
  List.map
  (fun a -> Tobj [("name",Tstr a.name);("color", Tstr (assignColor colorlist players a.name))])
    players

(* Makes an html file from the buildings given and saves it in mappage.html *)
let make_html build players= Jg_template.from_file "mappage.tmpl"
  ~models: (("Players", Tlist (make_player_list players))::(listofTuples build))
(* Makes corresponding css file for the html file generated above *)
let make_css build players= Jg_template.from_file "style.tmpl"
    ~models: (listOfColorTuples build players)


let write_html_file build players= Core.Std.Out_channel.write_all "game.html"
     ~data: (make_html build players)
>>>>>>> 6d89db72f1fa9b5c288d419b21d0b2e4e0c333da
let write_css_file build players = Core.Std.Out_channel.write_all
    "gameStyle.css" ~data:(make_css build players)
