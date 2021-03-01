(* Zadanie 1: Arytmetyka *)
(* Karolina Drabik*)
(* code review: Paweł Putra *)

(* Typ reprezentujący niedokładne wartości - lista przedzialow w postaci par liczb rzeczywistych*)
type wartosc = 
	(float * float) list;;

(*----------procedury pomocnicze do porzadkowania list przedzialow------------------*)

(*nadaje odpowiedni znak 0, jesli jest ono krancem przedzialu x*)
let normalizuj x =
	match x with 
	| (a, b) -> if a = 0. then ((+0.), b)
              else if b = 0. then (a, (-0.))
                   else (a, b);;

(*sprawdza czy x = nan*)
let is_nan x = compare x nan = 0;;	

(*sortuje liste przedziałów l1, zwraca liste przedzialow posortowanych rosnaco*)
let sortuj l1 =
	List.sort compare l1;;
	
(*scala przedzialy x y w jeden, przy zalozeniu ze x < y*)
let scal x y = 
	match x, y with
	| (a, b), (c, d) -> (a, d);;
	
(*sprawdza, czy przedzialy x y na siebie nachodza, przy zalozeniu ze x < y*)
let nachodza x y = 
    match x, y with
    | (a, b), (c, d) -> if c <= b then true (*x jest koniecznie pierwszy po posortowaniu*)
                        else false;;
					  
(*procedura pomocnicza do porzadkowania listy przedzialow*)
(* sortuje, scala nachodzace na siebie przedzialy, dzieli przedzialy zawierajace 0 na 2 czesci*)
(*akt - aktualnie rozpatrywany przedzial, nowa - uporzadkowana lista wyjsciowa odwrocona, l -lista wejsciowa*)          
let rec bezpierwszej l akt nowa = 
	match l, akt with
	| [], (pocz, kon) -> if 0. > pocz && 0. < kon 
                      then ((+0.), kon)::(pocz, (-0.))::nowa
                      else (normalizuj akt)::nowa 
	| (h::t), (pocz, kon) -> if (nachodza akt h) 
                         then bezpierwszej t (scal akt h) nowa
                         else if 0. > pocz && 0. < kon 
                              then bezpierwszej t h (((+0.), kon)::(pocz, (-0.))::nowa)
                              else bezpierwszej t h ((normalizuj akt)::nowa);;
	
(*porzadkuje liste przedzialow l1, zwraca uporzadkowana liste przedzialow odwrocona*)
let porzadkuj l1 nowa =
	match l1 with 
	| [] -> []
	| akt::l -> bezpierwszej l akt nowa;;
	
(*-----------------------------------------------------------------------------------*)

(*Konstruktory*)

let wartosc_dokladnosc x p = 
	List.rev (porzadkuj ([( min ( x +. x *. (p /. 100.) ) ( x -. x *. (p /. 100.) ), max ( x +. x *. (p /. 100.) ) ( x -. x *. (p /. 100.) ) )] ) []);;
	
let  wartosc_od_do a b = 
	List.rev ( porzadkuj ( [(min a b, max a b)] : wartosc) []);;
	
let wartosc_dokladna x =
	( [normalizuj (x, x)] : wartosc);;
  
(*Selektory*)
	
let min_wartosc x =
	match x with
	| ((a, b)::t) -> a
	| ([] : wartosc) -> nan;; (*dla pustej zwracam nan*)
	
let rec max_wartosc x =
	match x with
	| (a, b)::[] -> b
	| (a, b)::w -> (max_wartosc w)
	| [] -> nan;; (*dla pustej listy zwracam nan*)
	
let sr_wartosc x =
	((min_wartosc x) +. (max_wartosc x)) /. 2.;;
	
let rec in_wartosc x y = 
	match x with
	| (a, b)::w -> if y >= a && y <= b then true
								 else (in_wartosc w y)
	| [] -> false;; (*dla pustej listy zwracam nan*)
	
(*-----------------------------------------------------------------------------------*)

(* dodawanie *)

(*dodaje do siebie dwa przedzialy x y, zwraca przedzial*)	
let dodaj x y = 
	match x, y with
	| (a, b), (c, d) -> (a +. c, b +. d);;
	
(*dodaje pojedynczy przedzial x do wszystkich przedzialow z listy l, zwraca liste przedzialow acc*)	
let rec przedzial_do_listy x l acc =
	match l with
	| h::t -> (przedzial_do_listy x t ((dodaj x h)::acc) )
	| [] -> acc;;

(*dodaje do siebie wartosci w postaci list przedzalow l1 l2, zwraca liste przedzialow acc*)	
let rec dodaj_listy l1 l2 acc =
	match l1 with
	| h::t -> dodaj_listy t l2 ( (przedzial_do_listy h l2 [])@acc )(*nowy akumulator*)
	| [] -> acc;;

(* wykonuje dodawanie na wartosciach x y w postaci list przedzalow*)
(*zwraca wynik w postaci uporzadkowanej listy przedzialow*)
let plus x y =
	 List.rev ( porzadkuj ( sortuj( dodaj_listy x y [])) [] );;
	 
(*-----------------------------------------------------------------------------------*)

(* mnożenie *)

(*zwraca maksimum z dwoch liczb rzeczywistych a b, ktore moga byc nan*)
let maksimum a b =
	if is_nan a then b
	else if is_nan b then a
       else max a b;;
		 
(*zwraca minimum z dwoch liczb rzeczywistych a b, ktore moga byc nan*)
let minimum a b =
	if is_nan a then b
	else if is_nan b then a
       else min a b;;

(*mnoży przez siebie dwa przedzialy x y, zwraca przedzial*)	
let mnoz x y = 
	match x, y with
	| (a, b), (c, d) -> 
		( (minimum (minimum (a *. c) (a *. d)) (minimum ( b*.c ) (b *. d))),   
		(maksimum (maksimum (a *. c) (a *. d)) (maksimum ( b *. c ) (b *. d))) );;
	
(*mnoży pojedynczy przedzial x ze wszystkimi przedziałami z listy l, zwraca liste przedzialow acc*)	
let rec przedzial_razy_lista x l acc =
	match l with
	| h::t -> (przedzial_razy_lista x t ((mnoz x h)::acc) )
	| [] -> acc;;

(*mnozy przez siebie wartosci w postaci list przedzalow l1 l2, zwraca liste przedzialow acc*)	
let rec mnoz_listy l1 l2 acc =
	match l1 with
	| h::t -> mnoz_listy t l2 ( (przedzial_razy_lista h l2 [])@acc )(*nowy akumulator*)
	| [] -> acc;;

(*mnozy przez siebie dwie wartosci w postaci list przedzialow x y*)
(*zwraca wynik w postaci uporzadkowanej listy przedzialow*)
let razy x y =
	 List.rev ( porzadkuj ( sortuj( mnoz_listy x y [])) [] );;
	 
(*-----------------------------------------------------------------------------------*)

(* odejmowanie *)

(* wykonuje odejmowanie na wartosciach w postaci list przedzialow x y*)
(*zwraca wynik w postaci uporzadkowanej listy przedzialow*)
let minus x y =
	 List.rev (porzadkuj ( sortuj( dodaj_listy x (razy [(-1., -1.)] y) [])) []);;
	 
(*-----------------------------------------------------------------------------------*)

(* dzielenie *)

(* dzieli przez siebie dwa przedzialy x y, zwraca przedzial *)	
let dziel x y = 
	match x, y with
	| (a, b), (c, d) -> if c = 0. && d = 0. then (nan, nan)
					  else ( 
					  (minimum (minimum (a /. c) (a /. d)) (minimum ( b/.c ) (b /. d))), 
            (maksimum (maksimum (a /. c) (a /. d)) (maksimum ( b /. c ) (b /. d))));;
	
(*dzieli pojedynczy przedzial x przez wszystkie przedzialy z listy l, zwraca liste przedzialow acc*)
let rec przedzial_przez_lista x l acc =
	match l with
	| h::t -> (przedzial_przez_lista x t ((dziel x h)::acc) )
	| [] -> acc;;

(*wykonuje dzielenie na listach przedzialow l1 l2, zwraca liste przedzalow acc*)
let rec dziel_listy l1 l2 acc =
	match l1 with
	| h::t -> dziel_listy t l2 ( (przedzial_przez_lista h l2 [])@acc )(*nowy akumulator*)
	| [] -> acc;;

(*dzieli przez siebie dwie wartosci w postaci list przedzialow*)
(*zwraca wynik w postaci uporzadkowanej listy przedzialow*)
let podzielic x y =
	 List.rev ( porzadkuj ( sortuj( dziel_listy x y [])) [] );;

(*-----------------------------------------------------------------------------------*)

(* Testy *)

(*
let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let c = plus a a
let d = razy a a
let e = podzielic a a
let f = minus a a;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));
assert ((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (* nan *)
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; (* <-0.1, 2.1> *)
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  (* <-3.0, 0.0> *)
let b = wartosc_od_do 0. 1.      (* <-0.0, 1.0> *)
let c = podzielic a b;;          (* (-inf, 0> *)
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

(* uwaga, ten test moze sie zawiesic przy pewnych implementacjach! *)
let a = wartosc_od_do (-2.) 3.
let b = wartosc_od_do 2. 3.
let c = podzielic b a

let rec iteruj f n acc = match n with
    | 0 -> acc
    | n when n > 0 -> iteruj f (n-1) (f acc acc)
    | _ -> acc

let x = iteruj razy 10 c;;
assert (not (in_wartosc x 0.));;
*)