(* Zadanie 6 - Przelewanka *)
(* Karolina Drabik *)
(* Code rewiew: Adam Bac *)


(* NWD liczb a i b, jesli obie rowne 0 zwraca 0. *)
let rec nwd a b =
  if a = 0 then b
  else if b = 0 then a
  else nwd b (a mod b)


(* Sprawdzam czy dany uklad jest potencjalnie do uzyskania
   tab - tablica wejsciowa pojemnosci szklanek
   i oczekiwanych poziomow wody. *)
let sprawdz tab =
  let n = Array.length tab in
  let wyn = ref false in
  let d = ref 0 in

  for i = 0 to n - 1 do
    let x, y = tab.(i) in
    d := nwd !d x; (* Licze nwd wszystkich pojemnosci. *)
    (* Sprawdzam od razu czy ktoras z oczekiwanych
       jest pelna lub pusta. *)
    if y = 0 || y = x then wyn := true
  done;

  (* Sprawdzam czy nwd pojemnosci dzieli
     wszystkie oczekiwane wartosci. *)
  for i = 0 to n - 1 do
    let x, y = tab.(i) in
    if !d <> 0 && y mod !d <> 0 then wyn := false
  done;
  !wyn


(* Biore pewien stan wody w szklankach oraz
   tab - tablice wejsciowa pojemnosci szklanek
   i oczekiwanych poziomow wody, napelniam szklanke nr i,
   zwracam nowy stan. *)
let nalej stan tab i =
  let nowa = Array.copy stan in
  nowa.(i) <- fst tab.(i);
  nowa


(* Biore pewien stan wody w szklankach oraz
   tab - tablice wejsciowa pojemnosci szklanek
   i oczekiwanych poziomow wody, oprozniam szklanke nr i,
   zwracam nowy stan. *)
let wylej stan nr =
  let nowa = Array.copy stan in
  nowa.(nr) <- 0;
  nowa


(* Biore pewien stan wody w szklankach oraz
   tab - tablice wejsciowa pojemnosci szklanek
   i oczekiwanych poziomow wody, przelewam mozliwie duzo wody
   ze szklanki nr i do szklanki nr j, zwracam nowy stan *)
let przelej stan tab i j =
  let nowa = Array.copy stan in
  let ile = min (fst tab.(j) - stan.(j)) stan.(i) in
  nowa.(i) <- stan.(i) - ile;
  nowa.(j) <- stan.(j) + ile;
  nowa


let przelewanka tab =
  if tab = [||] then 0 (* Na pewno mozliwe. *)
  else if not (sprawdz tab) then -1 (* Na pewno niemozliwe. *)
  else
    let n = Array.length tab in
    let ocz = Array.map (fun (x, y) -> y) tab in (* oczekiwany stan *)
    let akt = Array.make n 0 in (* poczatkowy stan *)
    if akt = ocz then 0
    else
      let mapa = Hashtbl.create 100 in
      Hashtbl.add mapa ocz 0;
      Hashtbl.add mapa akt 1;

      let licz = ref 2 in

      let q = Queue.create () in
      Queue.add (akt, 0) q; (* Dodaje do kolejki stan poczotkawy. *)

      let wyn = ref (-1) in

      let przetworz nowa krok =
        if not (Hashtbl.mem mapa nowa) then
          begin
            Queue.add (nowa, krok + 1) q;
            Hashtbl.add mapa nowa !licz;
            licz := !licz + 1
          end
        else if nowa = ocz then
          wyn := krok + 1
      in

      while !wyn = (-1) && not (Queue.is_empty q) do
        let stan, krok = Queue.take q in

        for i = 0 to n - 1 do
          przetworz (nalej stan tab i) krok
        done;

        if !wyn = (-1) then (* Jesli znalazlo, to nie sprawdza dalej. *)
          begin
            for i = 0 to n - 1 do
              przetworz (wylej stan i) krok
            done;

            if !wyn = (-1) then (* Jesli znalazlo to nie sprawdza dalej. *)
              for i = 0 to n - 1 do
                if !wyn = (-1) then (* Jesli znalazlo to nie sprawdza dalej. *)
                  for j = 0 to n - 1 do
                    if i <> j then przetworz (przelej stan tab i j) krok
                  done
              done
          end
      done;
      !wyn;;
      
(* Testy *)
(*
assert (przelewanka [| (10,2); (1,1) |] = 5);;
assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
  (0,0); (1,0) |] = (3));;
assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));;
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));;
assert (przelewanka [| (11,11); (11,1) |] = (-1));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
  (1,0); (0,0) |] = (3));;
assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
assert (przelewanka [| (310,76); (139,91) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
  (8));;
assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
  (1,0); (0,0) |] = (296971));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
assert (przelewanka [| (85,23); (524,210) |] = (-1));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;
assert (przelewanka [| (62,3); (38,7) |] = (-1));;
assert (przelewanka [| (15,15); (6,3); (42,32); (33,20) |] = (-1));;
assert (przelewanka [| (39,12); (35,34); (21,7); (2,1) |] = (-1));;
assert (przelewanka [| (1,0); (2,1); (2,1); (0,0); (2,0); (0,0); (0,0);
  (0,0); (1,1); (0,0); (1,0) |] = (4));;
assert (przelewanka [| (2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;
assert (przelewanka [| (2,0); (1,1); (1,1); (1,1); (0,0); (1,0); (3,2);
  (0,0) |] = (4));;
assert (przelewanka [| (1,1); (2,2); (4,1); (0,0); (1,0); (2,1) |] = (5));;
assert (przelewanka [| (1,0); (3,1); (2,2); (1,1); (1,0); (1,0) |] = (3));;
assert (przelewanka [| (20,7); (12,11) |] = (-1));;
assert (przelewanka [| (0,0); (21,21) |] = (1));;
assert (przelewanka [| (13,8); (11,11) |] = (14));;
assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
assert (przelewanka [| (4,4); (7,6); (2,2) |] = (6));;
assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
assert (przelewanka [| (0,0); (2,0); (0,0); (2,0); (3,2); (2,1); (1,0) |] =
  (3));;
*)