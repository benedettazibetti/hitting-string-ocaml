# Progetto: Hitting String (Best First) in OCaml

## Testo dell'Esercizio
Si consideri un insieme finito $S$ di stringhe di lunghezza $N$ sull’alfabeto $\{0,1,2\}$.
L'obiettivo è determinare, se esiste, una stringa $x$ di lunghezza $N$ sull’alfabeto $\{0,1\}$ tale che per ogni stringa $s \in S$, $d(s,x) > 0$, dove $d$ rappresenta la distanza di Hamming.

Il problema deve essere risolto utilizzando una ricerca di tipo best-first. Come funzione di valutazione (euristica) viene utilizzato il numero di stringhe all'interno dell'insieme $S$ che hanno una distanza superiore a zero dalla soluzione parziale corrente.

---

## Spiegazione Generale
Sia $S$ un insieme finito e sia $x$ la stringa obiettivo. La condizione necessaria è che la distanza di Hamming di $x$ sia maggiore di zero rispetto a tutti gli elementi di $S$. 

L'insieme $S$ viene trattato come una lista di stringhe con caratteri appartenenti a $\mathbb{Z}_3$. La ricerca di $x$ avviene attraverso l'esplorazione di un grafo costruito mediante una funzione successori. Ogni nodo del grafo rappresenta una stringa con caratteri in $\mathbb{Z}_2$. Ogni successore di un nodo $x$ viene generato variando un singolo carattere della stringa stessa.

L’algoritmo implementato è una ricerca best-first guidata da una funzione di valutazione che quantifica quanti elementi dell’insieme $S$ possiedono una distanza di Hamming strettamente positiva rispetto alla stringa $x$ analizzata.

---

## Analisi Dettagliata delle Funzioni

### 1. Distanza di Hamming
La funzione preposta al calcolo della distanza di Hamming confronta due stringhe e restituisce il numero di posizioni in cui i caratteri differiscono.

```ocaml
exception LunghezzaDiversa;;
let distHamm s1 s2 =
  if String.length s1 <> String.length s2 then raise LunghezzaDiversa
	else
		let rec aux i dist =
			if i=String.length s1 then dist
			else
				let dist'=
					if s1.[i]=s2.[i] then dist
					else (dist+1)
		in aux (i+1) dist'
in aux 0 0;;
```

Il codice legge in input due stringhe s1 e s2. Per prima cosa verifica che abbiano stessa lunghezza (in caso contrario produce un’eccezione definita precedentemente LunghezzaDiversa). 
Se s1 e s2 hanno la stessa lunghezza, tramite ricorsione, studia i singoli caratteri delle stringhe. 
Se sono uguali lascia dist invariato, se sono diversi incrementa di 1 il contatore dist (distanza di Hamming). 
Il caso terminale avviene quando i è pari alla lunghezza della stringa s1 (che è uguale alla lunghezza di s2), cioè nel momento in cui il codice ha già studiato tutti i caratteri della stringa. 
L’output del codice è dist, che determina la distanza di Hamming tra le due stringhe.

*OSS*: Per il nostro codice di ricerca verificare, all’interno della funzione distHamm, che le due stringhe abbiano la stessa lunghezza è inutile (è un controllo che avviene già nel corpo del problema in searchbf) ma è importante per garantire l’utilizzo di questa funzione in ogni caso possibile.

---

### 2. Funzione di valutazione
Tramite la distranza di Hamming si può studiare la funzione di valutazione che verrà poi utilizzata all’interno della ricerca vera e propria del problema come funzione euristica per la ricerca best first:

```ocaml
let funval s x =
  let rec conta s acc =
    match s with
    | [] -> acc
    | t::c -> 
        let acc2 = if (distHamm t x) > 0 then (acc + 1) else acc
        in conta c acc2
  in conta s 0;;
```

Il codice legge in input una lista di stringhe s e una stringa x, tramite ricorsione e pattern matching conta il numero di stringhe di s che hanno distanza di Hamming >0 da x.
Se il codice ha già analizzato tutti gli elementi di s restituisce l’accumulatore acc, altrimenti studia la distanza di Hamming della testa della lista (t) e, se è maggiore di 0, incrementa acc; ripete il pattern matching con la coda della lista.


---
### 3.Grafo
Per analizzare il problema considero un grafo costruito tramite funzione successione:

```ocaml
type 'a graph  = Graph of ('a -> 'a list);; 

exception NonValido;;
let succ x visited =
	if String.length x = 0 then raise NonValido
				else
				let n = String.length x in
  					let rec aux i acc =
    						if i = n then acc
    							 else
      							let new_x = String.sub x 0 i ^ (if x.[i] = '0' then "1" else "0") ^ String.sub x (i + 1) (n - i - 1) in 
					if not (List.mem new_x visited) then aux (i + 1) (new_x :: acc) 
      									else aux (i + 1) acc
  	in aux 0 [];;
```

Per costruire un grafo tramite successione prima di tutto definisco un nuovo tipo graph che associa ad un nodo di tipo ‘a una lista di nodi di tipo ‘a, nel nostro caso specifico ‘a sarà un tipo stringa.
Da un nodo x, l’obiettivo è quello di costruire un grafo che abbia come successori di x delle stringhe con distanza di Hamming con x uguale a 1 (cioè hanno un solo carattere della stringa diverso da x).
Innanzitutto, verifico che la stringa del nodo x abbia lunghezza maggiore di zero (in caso contrario il codice produce un’eccezione definita precedentemente NonValido), se la stringa è consistente allora il codice procede a trovare i suoi successori.
L’idea è quella di far scorrere la stringa x e definire new_x come una stringa uguale a x ma con l’elemento in i-esima posizione modificato (se x.[i]=0 allora new_x.[i]=1 e se x.[i]=1 allora new_x.[i]=0). Nella stringa di output avrò prima la stringa in cui modifico l’ultimo bit, poi a scalare fino all’ultimo elemento della stringa che è quello dove modifico il primo bit. La funzione List.mem permette di non avere nella lista nodi già visitati.
In Ocaml le stringhe sono fisse, per modificare x utilizzo quindi la funzione di sottostringa String.sub a b c che permette di considerare solo c elementi della stringa a a partire dall’elemento in posizione b.

**Nota**: potrei aggiungere un controllo in più (che, però, non è necessario) che effettivamente verifichi che la distanza di Hamming sia pari a 1 (infatti questa funzione qui non è mai utilizzata esplicitamente). Nel modo seguente:

```ocaml
let new_x = String.sub x 0 i ^ (if x.[i] = '0' then "1" else "0") ^ String.sub x (i + 1) (n - i - 1) in
      			(* Se la distanza di Hamming è 1, aggiungi new_x come successore *)
      					if distHamm x new_x = 1 then aux (i + 1) (new_x :: acc)
      								      else aux (i + 1) acc
  in aux 0 [];;
```
---

### 4. Validità stringhe
Nel nostro problema vogliamo che l’insieme s sia formato da stringhe tutte della stessa lunghezza e con caratteri in Z3. Creo una funzione che controlli la validità di s:

```ocaml
let svalido s =
	match s with
		[] -> true  
  		|e1 :: rest ->
      			let lunghezza_attesa = String.length e1 in
      				let stringa_valida e =
        				String.length e = lunghezza_attesa &&
        				let rec controlla i =
          					if i = String.length e then true
          								else match e.[i] with
            								'0' | '1' | '2' -> controlla (i + 1)
            								| _ -> false
        				in controlla 0
      			in List.for_all stringa_valida s;;
```
La funzione svalido controlla che la lista s s sia vuota oppure, nel caso non lo fosse, controlla che il suo primo elemento (e1) abbia uguale lunghezza alle altre stringhe della lista s e controlla che tutti i caratteri di e1 siano in Z3 (‘0’, ‘1’ oppure ‘2’). 
Il comando List.for_all permette di svolgere questo procedimento per tutti gli elementi della lista s.


Oltre alle condizioni su s, il nostro problema ne impone anche su x. Analizziamo la funzione di validità di x:
```ocaml
let xvalido x=
	let rec controlla i =
		if i=String.length x then true
				else 
				match x.[i] with
				'0' | '1' -> controlla (i+1)
				|_ -> false
	in controlla 0;;
```
Il codice prende in input la stringa x e restituisce un booleano. Tramite una ricorsione analizza ogni carattere della striga x. Se ognuno di essi è in Z2 allora restituisce true, se anche solo uno di essi non è uguale né a ‘0’ né a ‘1’ allora restituisce false.
Il caso terminale si ottiene, ovviamente, quando il contatore i è uguale alla lunghezza della stringa x (cioè sono stati controllati tutti gli elementi della stringa e nessuno ha restituito false).

---
### 5.Corpo del problema

Studio ora, passo per passo, il corpo del problema:

```ocaml
exception SoluzioneNonTrovata;;
exception InsiemeNonValido;;
exception InsiemeSVuoto;;
exception NodoInizioNonValido;;
```
Ho inizialmente dichiarato le varie possibili eccezioni dovute a controlli sulla lista s, sul nodo iniziale del grafo e su controlli incrociati tra essi.

```ocaml
let rec stampalista = function
	[] -> print_newline()
	|x::rest -> print_string(x); print_string(";"); stampalista rest;;
```

La funzione stamaplista permetterà di visualizzare i nodi che si devono ancora visitare. Singolarmente può essere utilizzata per stampare una qualsiasi lista.

```ocaml
let inserisci coda (x, f_val) =
	let rec aux acc = function
 		[] -> List.rev_append acc [(x, f_val)]
e     		| (y, f_val')::rest when f_val' < f_val -> List.rev_append acc ((x, f_val)::(y, f_val')::rest)
     		| t::rest -> aux (t::acc) rest
in aux [] coda;;
```

La funzione inserisci permette di ordinare una stringa x all’interno di una lista di stringhe a seconda della funzione di valutazione. Per una questione di costo computazionale, al posto di ordinare la lista dalla stringa con funzione valutazione più alta a quella più bassa, la ordina al contrario e poi la rovescia. 
Il ruolo della funzione List.rev_append è essenziale in questa parte di codice. List.rev_append è una funzione ottimizzata di Ocaml che permette di invertire la lista acc e concatenarla alla lista successiva in un solo passaggio. 

Avrei potuto considerare una versione meno efficiente del codice ma forse più leggibile:
```ocaml
let inserisci_meno_efficiente coda (x, f_val) =
let rec aux acc = function
   		[] -> List.rev acc @ [(x, f_val)]
    		| (y, f_val')::rest when f_val' < f_val -> List.rev acc @ ((x, f_val)::(y, f_val')::rest)
    		| t::rest -> aux (t::acc) rest
in aux [] coda;;
```
Questo codice svolge lo stesso compito di quello precedente ma utilizzando sia List.rev che @. Ho, quindi, un costo computazionale molto più alto.



```ocaml
let tuttialmenoundue s=
	List.for_all (fun e ->
	String.exists (fun d -> d = '2') e
	) s;;
```
La funzione tuttialmenoundue considera l’insieme finito s e analizza i suoi elementi. Studia se tutte le stringhe hanno il carattere ‘2’. Questa funzione sarà utile nel corpo del problema per escludere un caso particolare: se tutte le stringhe di s hanno almeno un ‘2’ ed x è una stringa valida allora sicuramente la distanza di Hamming da ogni elemento di s è maggiore di zero perché i caratteri di x sono in Z2. 

```ocaml
let searchbf s inizio =
	if not (svalido s) then raise InsiemeNonValido;
	if not (xvalido inizio) then raise NodoInizioNonValido;
	if s= [] then raise InsiemeSVuoto;

	let lunghezzae = String.length (List.hd s) in
	if String.length inizio <> lunghezzae then raise NodoInizioNonValido;


	if tuttialmenoundue s then inizio 
	else

	let rec codabf coda visited =
		let coda_da_stampare = List.map (fun (x, _) -> x) coda
		in stampalista coda_da_stampare
	
    	match coda with
      		[] -> raise SoluzioneNonTrovata  
    		| (x, _)::rest when funval s x = List.length s -> x 
    		| (x, _)::rest -> 
        				let successore = succ x visited 
					in let successore_valido = 
          					List.filter (fun x' -> not (List.mem x' visited)) successore 
					   in let coda' = 
          						List.fold_left (fun acc x' -> let f_val = funval s x' 
							in inserisci acc (x', f_val)) rest successore_valido 
	in codabf coda' (x :: visited) 
in  codabf [(inizio, funval s inizio)] [];;
```

La funzione searchbf considera in input un insieme finito s, considerato come una lista di stringhe, e una stringa inizio. L’obiettivo, come richiede il problema, è quello di trovare una stringa x tale che la sua distanza di Hamming da tutti gli elementi di s sia maggiore di zero.
Per prima cosa il codice verifica che s e inizio siano validi tramite le funzioni svalido e xvalido; verifica che s non sia un insieme vuoto, poiché in quel caso il problema è insignificante, e che la lunghezza della stringa inizio sia uguale alla lunghezza del primo elemento di s (estratto tramite List.hd, ha senso considerare solo il primo elemento perchè abbiamo verificato in svalido che tutti gli elementi di s hanno lunghezza uguale). Se non vale una di queste condizioni, il codice riporta un’eccezione (diversa a seconda della condizione non rispettata).

Per prima cosa si sviluppa il caso particolare in cui ogni elemento di s ha il carattere ‘2’. Avendo verificato che inizio sia una stringa valida (tramite xvalido), si può concludere direttamente che inizio, qualsiasi stringa sia, ha distanza di Hamming maggiore di zero con ogni elemento di s: è quindi l’output del nostro codice.

Studiamo ora il caso generale. Tramite la funzione ricorsiva codabf, che prende in input due liste coda e visited, implemento la ricerca best-first vera e propria.
Coda è una lista di coppie di striga e funzione di valutazione della stessa (questo fatto è chiaramente descritto dal fatto che codabf è richiamato su [(inizio, funval s inizio)] e []), visited è una lista di stringhe composta dai nodi già analizzati, così da evitare cicli e ripetizioni.
Dopo la stampa della coda, utilizzo il pattern matching per analizzare la coda:
-	se è la lista è vuota (caso terminale) vuol dire che il programma ha già analizzato tutti i possibili successori di inizio senza aver trovato soluzioni valide. Viene sollevata quindi l’eccezione SoluzioneNonTrovata;
-	se il primo elemento della coda ha distanza di Hamming maggiore di zero con tutti gli elementi di s (il valore della sua funzione di valutazione è uguale alla lunghezza della stringa s), x è la stringa cercata;
-	se x, la prima stringa della lista coda, non è ancora la soluzione, allora genero i successori di x e li inserisco nella lista coda in ordine decrescente di funzione di valutazione, tramite la funzione inseriscicoda. Un nuovo nodo viene inserito solo se non è presente nella lista visited (che è l’insieme dei nodi già visitati), List.filter è il comando che fa questa cosa.

---

### Esempi significativi
Considero degli esempi significativi per poter analizzare tutte le eccezioni del mio codice:

1.	Sia x non in Z2:
```ocaml
let s=["000";"100";"100";"010";"001";"011";"101";"110";"110";"121";"200";"202";"111"];;
# searchbf s "212";;
Exception: NodoInizioNonValido.
```

2.	Sia x di lunghezza diversa rispetto agli elementi di s:
```ocaml
let s=["000";"100";"100";"010";"001";"011";"101";"110";"110";"121";"200";"202";"111"];;
# searchbf s "100000";;
Exception: NodoInizioNonValido.
```

3.	Sia s un insieme vuoto:
```ocaml
let s4=[];;
# searchbf s4 "000";;
Exception: InsiemeSVuoto.
```

4.	Sia s non in Z3:
```ocaml
let s5=["0000";"1200";"1002";"0201";"2011";"1021";"1210";"1210";"2121";"2020";"2023"];;
# searchbf s5 "0000";;
Exception: InsiemeNonValido.
```

5.	Siano gli elementi di s di lunghezze diverse:
```ocaml
let s3=["00";"1200";"102";"10";"0201";"2011";"1021";"1210";"1210";"2121";"2020";"2022"];;
# searchbf s3 "001";;
Exception: InsiemeNonValido.
```

6.	Sia x valido e s un insieme con stringhe tutte contenenti il carattere ‘2’:
```ocaml
let s2=["2000";"1200";"1002";"0210";"0201";"2011";"1021";"1210";"1210";"2121";"2020"];;
	# searchbf s2 "0000";;
- : string = "0000"
```

7.	Sia x non trovabile:
```ocaml
let s=["000";"100";"100";"010";"001";"011";"101";"110";"110";"121";"200";"202";"111"];;
# searchbf s "000";;
000;
001;010;100;
010;011;101;100;
011;101;011;100;110;
101;011;111;100;110;
011;111;111;100;110;100;
111;111;111;100;110;100;
111;111;100;110;100;110;
111;100;110;100;110;110;
100;110;100;110;110;110;
110;100;110;110;110;110;
100;110;110;110;110;
110;110;110;110;
110;110;110;
110;110;
110;
Exception: SoluzioneNonTrovata.
```

8.	Considero per ultimo un caso generale:
```ocaml
let s1=["000";"100";"100";"010";"001";"011";"101";"110";"110";"121";"200";"202"];;
# searchbf s1 "000";;
000;
001;010;100;
010;011;101;100;
011;101;011;100;110;
111;101;011;100;110;
- : string = "111"

```





