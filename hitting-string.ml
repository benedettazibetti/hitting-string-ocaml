(* DISTANZA DI HAMMING *)
exception LunghezzaDiversa;;	

let distHamm s1 s2=
     if String.length s1 <> String.length s2 then raise LunghezzaDiversa
	else let rec aux i dist =
if i=String.length s1 then dist (*ho già studiato tutti gli elementi della stringa*)
			else let dist'= 
				if s1.[i]=s2.[i] then dist
					else (dist+1)
	in aux (i+1) dist'
      in aux 0 0;;(*Caso iniziale: elemento in posizione zero, distanza nulla*)


(* FUNZIONE DI VALUTAZIONE *)
let funval s x =
      let rec conta s acc=
match s with
		[] -> acc (*ho già studiato tutti gli elementi di s*)
		|t::c -> let acc2 =
				if (distHamm t x) > 0 then (acc+1)
							else acc
in conta c acc2 (*Ricorsione*)
   in conta s 0;; (*Caso iniziale, con accumulatore nullo*)



(* COSTRUZIONE GRAFO *)
type 'a graph  = Graph of ('a -> 'a list);; (*Grafo che associa ad un elemento una lista di succ*)

exception NonValido;;
let succ x visited =
   if String.length x = 0 then raise NonValido (*Voglio che la lunghezza di x sia maggiore di zero*)
	else let n = String.length x in
  		let rec aux i acc =
    			if i = n then acc (*Ho già effettuato il procedimento seguente su tutti i bit di x*)
else let new_x = String.sub x 0 i ^ (if x.[i] = '0' then "1" else "0") ^ String.sub x (i + 1) (n - i - 1)
in if not (List.mem new_x visited) then aux (i + 1) (new_x :: acc) (*Aggiunge il nuovo nodo solo se non è già stato visitato*)
      							       else aux (i + 1) acc
   in aux 0 [];; (*Parte dal primo elemento, in posizione zero e ha come accumulatore una lista vuota*)


(* FUNZIONI DI CONTROLLO *)
(*Verifico che tutti gli elementi di s abbiano la stessa lunghezza e che siano elementi di Z3*)
let svalido s =
   match s with
[] -> true  
  	|e1 :: rest -> let lunghezza_attesa = String.length e1 
in let stringa_valida e =
        				String.length e = lunghezza_attesa &&
        					let rec controlla i =
          						if i = String.length e then true
          							else match e.[i] with
            							'0' | '1' | '2' -> controlla (i + 1)
            							| _ -> false
        					in controlla 0
   in List.for_all stringa_valida s;;


(*Verifico che i caratteri della stringa x siano in Z2*)
let xvalido x=
   let rec controlla i =
if i=String.length x then true
		else match x.[i] with
			'0' | '1' -> controlla (i+1)
			|_ -> false
   in controlla 0;;



(* CORPO DEL PROBLEMA *)
exception SoluzioneNonTrovata;;
exception InsiemeNonValido;;
exception InsiemeSVuoto;;
exception NodoInizioNonValido;;

let rec stampalista = function
	[] -> print_newline()
	|x::rest -> print_string(x); print_string(";"); stampalista rest;;


(*Funzione di inserimento dei nodi nella coda in ordine decrecente di f_val*)
let inserisci coda (x, f_val) =
   let rec aux acc = function
 	[] -> List.rev_append acc [(x, f_val)]
     	| (y, f_val')::rest when f_val' < f_val -> List.rev_append acc ((x, f_val)::(y, f_val')::rest)
     	| t::rest -> aux (t::acc) rest
   in aux [] coda;;


(*Funzione che restiuisce true se tutti gli elementi di s contengono almeno un due*)
let tuttialmenoundue s=
	List.for_all (fun e -> String.exists (fun d -> d = '2') e) s;;



let searchbf s inizio =
	(*Analizzo prima tutti i casi degeneri *)
	if not (svalido s) then raise InsiemeNonValido;
	if not (xvalido inizio) then raise NodoInizioNonValido;
	if s= [] then raise InsiemeSVuoto;

	let lunghezzae = String.length (List.hd s) in
	if String.length inizio <> lunghezzae then raise NodoInizioNonValido;


	if tuttialmenoundue s then inizio (*Se inizio è una stringa valida, ne va bene una qualsiasi*)
	else

	let rec codabf coda visited =
		let coda_da_stampare = List.map (fun (x, _) -> x) coda
		in stampalista coda_da_stampare (*Stampo la coda dei nodi che devo visitare*);
	
    	match coda with
      		[] -> raise SoluzioneNonTrovata  (*Se la coda è vuota, non abbiamo trovato soluzione*)
    		| (x, _)::rest when funval s x = List.length s -> x 
(*Se la funzione di valutazione è n, significa che tutti gli elementi di s hanno distanza da x maggiore di 0 quindi la x è quella valida*)

    		| (x, _)::rest -> 
        				let successore = succ x visited 
					in let successore_valido = 
          					List.filter (fun x' -> not (List.mem x' visited)) successore 
					   in let coda' = 
          						List.fold_left (fun acc x' -> let f_val = funval s x' 
							in inserisci acc (x', f_val)) rest successore_valido 
	in codabf coda' (x :: visited)  (*Aggiungo x alla lista dei nodi visitati e che non rivisiteremo*)
in  codabf [(inizio, funval s inizio)] [];;
(*Il primo elemento che consideriamo nella coda è la stringa x e la sua funzione di valutazione con s*)
