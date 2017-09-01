(*** Tris en CAML ***)
let echange t i j = let a = t.(i) in
		t.(i) <- t.(j);
		t.(j) <- a;;

let tri_select_iter t =
	let n = Array.length t in
	let a = ref 0 in
	for i = 0 to (n-2) do
			a := i;
			for j = (i+1) to (n-1) do
				if t.(!a) > t.(j) then a := j
			done;
			echange t i (!a)
	done;;

let tri_bulles_iter t =
	let n = Array.length t in
	for i = 0 to (n-2) do
		for j = 0 to (n-i-2) do
			if t.(j) > t.(j+1) then echange t j (j+1)
		done;
	done;;

let tri_bulles t =
	let a = ref false in
	let n = Array.length t in
	while not !a do
		a := true;
		for i = 0 to (n-2) do
			if t.(i) > t.(i+1) then
				(echange t i (i+1);
				a := false);
		done;
	done;;

let tri_insert t =
	let n = Array.length t in
	let j = ref 0 in
	for i = 1 to (n-1) do
		j := i;
		while !j >= 1 && t.(!j) < t.(!j-1) do
			(echange t !j (!j-1);
			j := !j-1);
		done;
	done;;
