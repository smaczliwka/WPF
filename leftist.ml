(* Zadanie 2: Drzewa Lewicowe *)
(* Karolina Drabik *)
(* code review: Konrad Skublicki *)

(*typ złączalnej kolejki priorytetowej w postaci drzewa binarnego*)
(*węzeł - krotka: wartość węźle, lewe poddrzewo, prawe poddrzewo,
 długość skrajnie prawej ścieżki zaczynającej się w nim
 lub liść*)
type 'a queue =
  | Node of 'a * 'a queue * 'a queue * int
  | Leaf;;
  
(*pusta kolejka priorytetowa*)
let empty = Leaf;;

(*zwraca długość skrajnie prawej ścieżki zaczynającej się w węźle q*)
let dep q =
  match q with
  | Node (x, l, r, dep) -> dep
  | Leaf -> 0;;

(*zwraca złączenie kolejek q1 i q2*)
let rec join q1 q2 =
  match q1, q2 with 
  | Leaf, _ -> q2
  | _, Leaf -> q1
  | Node (x1, l1, r1, dep1), Node (x2, l2, r2, dep2) ->
    if x1 > x2 then join q2 q1
    else let nowy = join r1 q2 in
      if dep nowy < dep l1 then Node (x1, l1, nowy, (dep nowy) + 1)
      else Node (x1, nowy, l1, (dep l1) + 1);;
      
(*zwraca kolejkę powstałą z dołączenia elementu x do kolejki q*)
let add x q =
  let nowy =  Node (x, Leaf, Leaf, 1) in
  join nowy q;;

(*wyjątek podnoszony przez delete_min gdy kolejka jest pusta*)
exception Empty;;

(*dla niepustej kolejki q zwraca parę (e,q') gdzie 
  e jest elementem minimalnym kolejki q a q' to q bez elementu e*)
(*jeśli q jest pusta podnosi wyjątek Empty*)
let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node (x, l, r, dep) -> (x, join l r);;
  
(*zwraca true jeśli kolejka q jest pusta, w przeciwnym razie false*)
let is_empty q =
  match q with
  | Leaf -> true
  | Node (x, l, r, dep) -> false;;
    
(* TESTY *)

(*
let test a b num msg =
  if a = b then print_endline "ok"
  else (print_int num; print_endline msg);; 

let rec zwin l q num msg =
  try
    match l with
    | [] -> test q empty num msg
    | h::t -> let (mn,r) = delete_min q in test mn h num msg; zwin t r (num+1) msg
  with Empty -> (print_int num; print_string "Empty"; print_endline msg);;

let a = add 0. empty;;        (* 0.*)
let b = add 1. empty;;        (* 1. *)
let c = add (-0.1) empty;;    (* -0.1 *)
let d = add 7. a;;            (* 0., 7. *)
let e = add (-3.) d;;         (* -3., 0., 7. *)
let f = add (-0.5) c;;        (* -0.5, -0.1 *)
let g = join b c;;            (* -0.1, 1.*)
let h = join d e;;            (* -3., 0., 0., 7., 7. *)
let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)
let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)

let la = [0.];;
let lb = [1.];;
let lc = [-0.1];;
let ld = la @ [7.];;
let le = -3.::ld;;
let lf = -0.5::lc;;
let lg = lc @ lb;;
let lh = [-3.; 0.; 0.; 7.; 7.];;
let li = [-3.; -0.5; -0.1; 0.; 7.];;
let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;

test (join empty empty) empty (-1) ": empty + empty";;
zwin la a 0 ": a";;
zwin lb b 0 ": b";;
zwin lc c 0 ": c";;
zwin ld d 0 ": d";;
zwin le e 0 ": e";;
zwin lf f 0 ": f";;
zwin lg g 0 ": g";;
zwin lh h 0 ": h";;
zwin li i 0 ": i";;
zwin lj j 0 ": j";;
*)