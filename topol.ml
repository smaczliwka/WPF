(* Zadanie 4: Sortowanie topologiczne *)
(* Karolina Drabik *)
(* code review: Adrian Górecki *)

open PMap 

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(*zwraca trojke - mape z dodaną do niej wartoscia w, ktorej przypisuje licznik
  (jesli w nie ma w mapie wczesniej), mape odwrotna i nowy licznik 
  (liczbe elementow mapy)*)
let skaluj w (mapa, odw, licznik) =
  if mem w mapa = false then ( (add w licznik mapa), (add licznik w odw), (licznik + 1) )
  else (mapa, odw, licznik)

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
 dodaje do mapy wierzcholki przypisujac im numery (kolejne liczby naturalne),
 zwraca trojke - mape, mape odwrotna i licznik mapy*) 
let rec przejrzyj lista (mapa, odw, licznik) = 
  match lista with
  | [] -> (mapa, odw, licznik)
  | (v, l)::t ->
    match l with
    | [] -> przejrzyj t (skaluj v (mapa, odw, licznik))
    | lh::lt -> przejrzyj ((v, lt)::t) (skaluj lh (mapa, odw, licznik))
    
(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
  zwraca trójkę - graf zaleznosci (tablice list numerow wierzcholkow),
  tablice stopni wejscia wierzcholkow i mape do zamiany numerow wierzcholkow
  na wartosci wierzcholkow podane na wejsciu*)    
let tworzy lista = 
  let (mapa, odw, licznik) = przejrzyj lista (empty, empty, 0) in
  let tab = Array.make licznik [] in
  let indeg = Array.make licznik 0 in
  let rec graf lst =
    match lst with
    | [] -> (tab, indeg, odw)
    | (v, l)::t -> 
      match l with
      | [] -> graf t
      | lh::lt ->  (
        tab.(find v mapa) <- (find lh mapa)::(tab.(find v mapa));
        indeg.(find lh mapa) <- (indeg.(find lh mapa) + 1);
        graf ((v, lt)::t) )
   in
   graf lista;;

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)  
let topol lista =
  let q = Queue.create () in
  let (tab, indeg, odw) = tworzy lista in
  for i = 0 to (Array.length indeg - 1) do
    if indeg.(i) = 0 then (Queue.add i q;)
  done;
  let wyn = ref [] in
  while (Queue.length q) > 0 do
    let v = (Queue.take q) in
    while (List.length tab.(v)) > 0 do
      let u = (List.hd tab.(v)) in
      tab.(v) <- (List.tl tab.(v));
      indeg.(u) <- (indeg.(u) - 1);
      if indeg.(u) = 0 then
        Queue.add u q;  
    done;
    wyn := v::!wyn;
  done;
  for i = 0 to (Array.length indeg - 1) do
    if indeg.(i) > 0 then (raise Cykliczne)
  done;
  List.map (fun x -> find x odw) (List.rev !wyn)
  
(* TESTY *)
  
(* Autor: Marek Puzyna
 * Licence: Unlicensed
 * Original repo: https://github.com/Day1721/UW *)
(* 
let czy_cykliczne l =
   match (try (topol l) with
      Cykliczne -> []) with
         | [] -> true
         | _ -> false
let test input output =
   let rec loop a b f = function
      | [] -> false
      | h::t -> 
         if f then 
            if h = b then true 
            else loop a b f t
         else if h = a then loop a b true t 
            else loop a b f t
   and pom i a = function
      | [] -> (match i with
         | [] -> true
         | g::o -> pom o (fst g) (snd g))
      | h::t -> match (loop a h false output) with
         | true -> pom i a t
         | false -> false in
   pom (List.tl input) (fst (List.hd input)) (snd (List.hd input))
let a = [(1, [2]); (2, [3]); (3, [4]); (4, [1])]
let b = [(1, [2]); (2, [3]); (3, [4])]
let c = [('A', ['B'; 'C'; 'E']); ('D', ['F'; 'E'; 'G']); ('B', ['C'; 'D']);
   ('C', ['D'; 'F']); ('F', ['G'; 'H'])]
let d = [("zolty", ["niebieski"; "bialy"; "czarny"]); ("bialy", ["czarny"]); 
   ("czarny", []); ("czerwony", ["zielony"; "zolty"; "niebieski"; "czarny"])]
let e = [(1, [2; 5; 8; 3]); (5, [8; 6; 4; 7]); (7, [6; 9; 2]); (8, [6; 9; 3])]
let _ = assert(czy_cykliczne a);
        assert(not (czy_cykliczne b));
        assert(test b (topol b));
        assert(test c (topol c));
        assert(test (List.tl c) (topol (List.tl c)));
        assert(test d (topol d));
        assert(test e (topol e));
        assert(test (List.tl e) (topol (List.tl e)));
        assert(test (b @ e) (topol (b @ e)));
        assert(test (List.tl b @ e) (topol (List.tl b @ e)))
  *)      