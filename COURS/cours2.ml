(* Structure *)
type 'a arbre = | Vide
								| Noeud of 'a * 'a arbre * 'a arbre ;;


(* Get the tree root *)
let racine t = match t with
		| Vide -> failwith "ZERFGHJKLM"
		| Noeud(r,_,_) -> r;;

(* Get the number of nodes *)
let rec nb_noeuds t = match t with
		| Vide -> 0
		| Noeud(_,g,d) -> (nb_noeuds g) + (nb_noeuds d) + 1;;

(* Get number of leaves *)
let rec nb_feuilles t = match t with
		| Vide -> 0
		| Noeud(_,Vide,Vide) -> 1
		| Noeud(_,g,d) -> (nb_feuilles g) + (nb_feuilles d);;

(* Get tree's height *)
let rec hauteur t = match t with
		| Vide -> 0
		| Noeud(_,g,d) -> max (hauteur g) (hauteur d);;
