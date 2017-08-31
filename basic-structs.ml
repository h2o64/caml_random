(*** Structures de base ***)

(** Pile - Mutable **)
type 'a pile = {mutable liste : 'a list}
let pilevide () = {liste = []};;
let empile a pile = pile.liste<-a::pile.liste;;
let depile pile = match pile.liste with
		| [] -> failwith "Tu peux pas dépiler si t'es vide"
		| h::t -> pile.liste<-t;;

(** File - Implementation arriere/avant - DEPRECEATED **)
type 'a file = {avant : 'a list ; arriere : 'a list}
let filevide () =  {avant = []; arriere = []};;
let enfile a file = {avant = file.avant ; arriere = a::file.arriere};;
let defile a file = match file.avant with
		| [] -> {avant = inversion file.arriere; arriere = []}
		| h::t -> {avant = t; arriere = file.arriere};;


(** File - Implementation arriere/avant - Mutable **)
type 'a file = {mutable avant : 'a list ; mutable arriere : 'a list}
let filevide () =  {avant = []; arriere = []};;
let enfile a file = file.arriere<-a::file.arriere;;
let defile a file = match file.avant with
		| [] -> file.avant<-inversion file.arriere;
						file.arriere<-[]
		| h::t -> file.avant<-t;;
































