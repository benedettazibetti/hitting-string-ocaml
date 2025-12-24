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
'''

Il codice legge in input due stringhe s1 e s2. Per prima cosa verifica che abbiano stessa lunghezza (in caso contrario produce un’eccezione definita precedentemente LunghezzaDiversa). 
Se s1 e s2 hanno la stessa lunghezza, tramite ricorsione, studia i singoli caratteri delle stringhe. 
Se sono uguali lascia dist invariato, se sono diversi incrementa di 1 il contatore dist (distanza di Hamming). 
Il caso terminale avviene quando i è pari alla lunghezza della stringa s1 (che è uguale alla lunghezza di s2), cioè nel momento in cui il codice ha già studiato tutti i caratteri della stringa. 
L’output del codice è dist, che determina la distanza di Hamming tra le due stringhe.

OSS: Per il nostro codice di ricerca verificare, all’interno della funzione distHamm, che le due stringhe abbiano la stessa lunghezza è inutile (è un controllo che avviene già nel corpo del problema in searchbf) ma è importante per garantire l’utilizzo di questa funzione in ogni caso possibile.

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

Il codice legge in input una lista di stringhe s e una stringa x, tramite ricorsione e pattern matching conta il numero di stringhe di s che hanno distanza di Hamming >0 da x.
Se il codice ha già analizzato tutti gli elementi di s restituisce l’accumulatore acc, altrimenti studia la distanza di Hamming della testa della lista (t) e, se è maggiore di 0, incrementa acc; ripete il pattern matching con la coda della lista.











