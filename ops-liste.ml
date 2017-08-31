(*** Opération sur les listes ***)

let liste_test = [4;5;6;7;8;9]

let test liste = match liste with
		| [] -> failwith "Liste vide"
		| h::t -> h;;

let increment a = a + 1;;

let rec parcours op liste = match liste with 
		| [] -> []
		| h::t -> (op h)::(parcours op t);;

let rec longueur liste = match liste with
		| [] -> 0
		| h::t -> 1 + longueur t;;

let rec maxi liste = match liste with
		| [] -> failwith "Error"
		| [a] -> a
		| h::t -> let b = maxi t in
								if h >= b then h
								else b;;

let rec element n liste = match liste with
		| [] -> failwith "Element not found"
		| h::t -> if n = 1 then h else element (n-1) t;;

let rec suppr x liste = match liste with
		| [] -> []
		| h::t -> if x = h then (suppr x liste)
							else h::(suppr x liste);;

let rec insere x n liste = if n = 1 then x::liste else
														match liste with
		| [] -> failwith "Error"
		| h::t -> h::(insere x (n-1) liste);;

let rec vider l1 l2 = match l1 with
		| [] -> l2
		| h::t -> vider t (h::l2);;

let inversion liste = vider liste [];;
let concat l1 l2 = vider (inversion l1) l2;;
