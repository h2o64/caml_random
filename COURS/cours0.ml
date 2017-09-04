(*** Exponentiation rapide ***)
(* val puiss : int -> int -> int = <fun> *)
let rec puiss x n =
	if n = 0 then 1
	else
		let y = puiss (x*x) (n/2) in
		if n mod 2 = 0 then y
		else x*y;;

(*** Tri rapide ***)
(* val scinder : 'a list -> 'a -> 'a list * 'a list = <fun> *)
let rec scinder s x = match s with
		| [] -> ([],[])
		| h::t -> let (a,b) = scinder t x in
							if h < x then (h::a,b)
							else (a,h::b);;

(* val trier : 'a list -> 'a list = <fun> *)
let rec trier s = match s with
		| [] -> []
		| h::t -> let (a,b) = scinder t h in (trier a)@(h::trier b);;
