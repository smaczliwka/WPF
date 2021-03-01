(* Zadanie 3: Origami *)
(* Karolina Drabik *)
(* code review: Grzegorz Kopania, grupa 1 *)

(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

let epsilon = 0.000000001

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
  o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
  a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
  od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
  (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
  w pozostałych przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) =
  let czy (x, y) =
    if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
    else 0
    in
 
  if x1 <= x2 && y1 <= y2 then (czy: kartka)
  else failwith "Złe wymiary kartki"
  
(*zwraca kwadrat liczby x*)  
let kwadrat x = x *. x
  
(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte
  o środku w punkcie [p] i promieniu [r] *)
let kolko (x1, y1) r =
  let czy (x, y) = 
    if kwadrat (x -. x1) +. kwadrat (y -. y1) <= kwadrat r +. epsilon then 1
    else 0
    in
  (czy: kartka)
  
(* zwraca wektor w postaci (x, y) mając dane współrzędne początku (x1, y1)
  i końca (x2, y2) *)  
let zrob_wektor (x1, y1) (x2, y2) =
  (x2 -. x1, y2 -. y1)
  
(*iloczyn wektorowy wektorów a i b  
 dodatni jeśli b po lewej stronie a 
 ujemny jeśli b po prawej stronie a 
 0 jeśli a i b współliniowe *)
let iloczyn a b =
  match a, b with (ax, ay), (bx, by) ->
    ax *. by -. ay *. bx
 
(*mając dane współrzędne dwóch punktów (x1, y1) (x2, y2) 
  zwraca prostą k: y = ax + b 
  przechodzącą przez te dwa punkty w postaci pary (a, b)
  przy założeniu, że x1, x2 są różne *)    
let prosta (x1, y1) (x2, y2) = 
  let a = (y2 -. y1) /. (x2 -. x1) in
  let b = y1 -. (a *. x1) in
  (a, b) 

(*mając daną prostą k: y = ax + b w postaci pary (a, b) i punkt (x, y)
 zwraca prostą l: y = a2x + b2 prostopadłą do niej 
 i przechodzącą przez punkt (x, y) w postaci pary (a2, b2)
 przy założeniu, że a różne od 0 - prosta k nie jest równoległa do osi x *)  
let prostopadla (a, b) (x, y) = 
  let a2 = -1. /. a in
  let b2 = y -. (a2 *. x) in
  (a2, b2)
  
(*mając dane dwie proste k: y = a1x + b1 i l: y = a2x + b2 
  w postaci (a1, b1) (a2, b2) zwraca punkt ich przecięcia 
  przy założeniu, że a1 i a2 są różne - proste nie są równoległe*)
let przeciecie (a1, b1) (a2, b2) = 
  let x = (b2 -. b1) /. (a1 -. a2) in
  let y = a1 *. x +. b1 in
  (x, y)

(*mając dany punkt (x, y) i parę punktów ((x1, y1), (x2, y2)) 
  przez które przechodzi prosta k zwraca obraz punktu (x, y)
  w symetrii osiowej względem prostej k*) 
let odbicie (x, y) ((x1, y1), (x2, y2)) =
  if x1 -. x2 <= epsilon && x1 -. x2 >= (-1.)*.epsilon then 
    (2. *. x1 -. x, y) (*prosta pionowa*)
  else if y1 -. y2 <= epsilon && y1 -. y2 >= (-1.)*.epsilon then 
    (x, 2. *. y1 -. y) (*prosta pozioma*)
  else (*robimy równanie prostej*)
    let p = prosta (x1, y1) (x2, y2) in
    let q = prostopadla p (x, y) in
    let (z, t) = przeciecie p q in 
    let (vx, vy) = zrob_wektor (x, y) (z, t) in
    (x +. 2. *. vx, y +. 2. *. vy)

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
  punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
  w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
  jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
  przebicie po prawej stronie prostej powinno więc zwrócić 0.
  Przebicie dokładnie na prostej powinno zwrócić tyle samo,
  co przebicie kartki przed złożeniem. Po stronie lewej -
  tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
  który nałożył się na punkt przebicia. *)    
let zloz (x1, y1) (x2, y2) k =
  let a = zrob_wektor (x1, y1) (x2, y2) in
  let czy (x, y) = 
    let b = zrob_wektor (x1, y1) (x, y) in
    if iloczyn a b <= epsilon && iloczyn a b >= (-1.)*.epsilon then k (x, y)
    else if iloczyn a b < (-1.)*.epsilon then 0
         else k (x, y) + k (odbicie (x, y) ((x1, y1),(x2, y2)))
  in
  (czy: kartka)
  
(*procedura pomocnicza - zloz z odwróconą kolejnością argumentów*)  
let pom acc element = 
  match element with (p1, p2) -> zloz p1 p2 acc
  
(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
  zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
  czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
  z listy lst*)  
let skladaj lst k =
  List.fold_left pom k lst
  
(* TESTY
  
(* Autor: Andrzej Głuszak 
 * Licence: Unlicensed 
 * Oryginal repo: https://gitlab.com/agluszak/mimuw-wpf-testy *)

let test a b msg = if a<>b then (print_int a; print_string "<>"; 
print_int b; print_string " test: "; print_endline msg);;

let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
	  ((5., 0.), (10., 5.));
	  ((10., 0.), (0., 10.));
	  ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
	  ((6., 0.), (10., 4.));
	  ((4., 0.), (10., 6.));
	  ((2., 0.), (10., 8.));
	  ((0., 0.), (10., 10.));
	  ((0., 2.), (8., 10.));
	  ((0., 4.), (6., 10.));
	  ((0., 6.), (4., 10.));
	  ((0., 8.), (2., 10.))];;

let p2 = skladaj l1 p1
let p3 = skladaj l2 p1
let k2 = skladaj l1 k1;;

test (p2 (7., 3.)) 0 "0.1: p2";;
test (p2 (5., 8.)) 0 "0.2: p2";;
test (p2 (3., 5.)) 0 "0.3: p2";;
test (p2 (5., 5.)) 0 "0.4: p2";;
test (p2 (0., 0.)) 2 "1: p2";;
test (p2 (0., 10.)) 2  "2: p2";;
test (p2 (2.5, 2.5)) 2 "3: p2";;
test (p2 (2.5, 7.5)) 2 "4: p2";;
test (p2 (2.5, 5.)) 4 "5: p2";;
test (p2 (0., 5.)) 5 "6: p2";;
test (p2 (1., 2.)) 4 "7: p2";;
test (p2 (1., 5.)) 8 "8: p2";;
test (p2 (1., 8.)) 4 "9: p2";;

test (k2 (7., 3.)) 0 "0.1: k2";;
test (k2 (5., 8.)) 0 "0.2: k2";;
test (k2 (3., 5.)) 0 "0.3: k2";;
test (k2 (5., 5.)) 0 "0.4: k2";;
test (k2 (2.5, 2.5)) 2 "1: k2";;
test (k2 (2.5, 7.5)) 2 "2: k2";;
test (k2 (2.5, 5.)) 4 "3: k2";;
test (k2 (0., 5.)) 5 "4: k2";;
test (k2 (1., 3.)) 4 "5: k2";;
test (k2 (1., 5.)) 8 "6: k2";;
test (k2 (1., 7.)) 4 "7: k2";;

test (p3 ((-4.), 6.)) 2 "1: p3";;
test (p3 ((-3.), 5.)) 1 "2: p3";;
test (p3 ((-3.), 7.)) 2 "3: p3";;
test (p3 ((-2.), 6.)) 3 "4: p3";;
test (p3 ((-2.5), 6.5)) 4 "5: p3";;
test (p3 ((-2.), 8.)) 4 "6: p3";;
test (p3 ((-1.), 7.)) 3 "7: p3";;
test (p3 ((-1.5), 7.5)) 6 "8: p3";;
test (p3 (0., 8.)) 5 "9: p3";;
test (p3 ((-1.), 9.)) 4 "10: p3";;
test (p3 ((-0.5), 8.5)) 8 "11: p3";;
test (p3 (0., 10.)) 6 "12: p3";;
test (p3 (1., 9.)) 5 "13: p3";;
test (p3 (0.5, 9.5)) 10 "14: p3";;

let kolo = kolko (0.,0.) 10. in
assert (kolo (1000., 0.) = 0);
let poziomo = zloz (0.,0.) (1.,0.) kolo in
assert (poziomo (0.,0.) = 1);
assert (poziomo (0.,1.) = 2);
assert (poziomo (0.,-1.) = 0);
let pionowo = zloz (0.,0.) (0.,1.) kolo in
assert (pionowo (0.,0.) = 1);
assert (pionowo (-1.,0.) = 2);
assert (pionowo (1.,0.) = 0);
let cwiartka = zloz (0.,0.) (0.,1.) poziomo in
assert (cwiartka (0.,0.) = 1);
assert (cwiartka (-1.,1.) = 4);
assert (cwiartka (-1.,0.) = 2);
*)
