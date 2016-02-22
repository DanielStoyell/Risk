{shared{
  open Eliom_lib
  open Eliom_content
  open Svg.D
}}


module Hello_app =
  Eliom_registration.App (
    struct
      let application_name = "hello"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Hello_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"The Game of Risk"
           ~css:[["css";"hello.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome to Cornell!"];
             img ~alt:("Risk Map")
             ~src:(make_uri
              ~service:(Eliom_service.static_dir ())
              ["risk_map.png"]) ();

          
           ])))

