(*** Tree structure ***)
type 'a arbre = | ArbreVide
								| Noeud of 'a * ('a arbre) * ('a arbre);;
let christmas_tree = Noeud(1,Noeud(2,Noeud(3,ArbreVide,ArbreVide),Noeud(4,ArbreVide,ArbreVide)),Noeud(5,Noeud(6,ArbreVide,ArbreVide),Noeud(7,ArbreVide,ArbreVide)))

let etiquette a = match a with
			| ArbreVide -> failwith "I'm not a fucking tree man"
			| Noeud(e,_,_) -> e;;
let fils_gauche a = match a with
			| ArbreVide -> failwith "I'm not a fucking tree man"
			| Noeud(_,g,_) -> g;;
let fils_droit a = match a with
			| ArbreVide -> failwith "I'm not a fucking tree man"
			| Noeud(_,_,d) -> d;;

let rec taille a = match a with
			| ArbreVide -> 0
			| Noeud(_,g,d) -> 1 + taille g + taille d;;

let rec hauteur a = match a with
			| ArbreVide -> 0
			| Noeud(_,g,d) -> 1 + max (taille g) (taille d);;

let rec nb_feuilles a = match a with
			| ArbreVide -> 0
			| Noeud(_,ArbreVide,ArbreVide) -> 1
			| Noeud(_,g,d) -> nb_feuilles g + nb_feuilles d;;

let rec search x a = match a with
			| ArbreVide -> false
			| Noeud(e,g,d) -> (e = x) || (search x g) || (search x d)
