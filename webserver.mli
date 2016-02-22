open Unix
open Str

type socket

(* Given a url link, will split it into the host and location of the file *)
val split_url: string -> string * string

val read_page : string -> unit

(* when clicking on a country on the webpage, actions need to take place *)
val install_tcp_server_socket: string

(* will actually do the running of the server *)
val server: unit -> unit

(* read everything pending in the socket *) 
val read_all: socket ->string

(*write to a socket *)
val write_all: socket->string->unit


