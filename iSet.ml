(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 

*)

(** Author : Karolina Drabik *)
(** Most of the code based on PSet - Polymorphic sets
    Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl *)
(** Code Review: Igor Witkowski *)

(** Type of interval set *)
type t =
  (*Set contains left son, interval (a, b) (assumes a < b),
    right son, height and number of elements*)
  | Node of t * (int * int) * t * int * int 
  (*Set is empty*)
  | Empty 

(** The empty set *)  
let empty = Empty

(** Returns true if the set is empty. *)
let is_empty x =
  x = Empty

(** Returns height of the given set. *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(** Returns number of elements in the given set. *)  
let count = function
  | Node (_, _, _, _, c) -> c
  | Empty -> 0

(** Adds two integers a, b. Returns max_int if a + b >= max_int. *)
let plus a b =
  if a>0 && max_int - a <= b then max_int
  else a + b

(** Adds numbers of elements of two sets x, y *)  
let plus_count x y =
  plus (count x) (count y)

(** Subtracts two integers a, b. Assumes a >= b.
    Returns max_int if a - b >= max_int *)  
let minus a b =
  if b != min_int then plus a (-b)
  else if a >= max_int + min_int then max_int
  else a - b

(** Returns number of elements in interval k. Assumes k2 >= k1*)  
let length k =
  match k with (k1, k2) -> 
    plus (minus k2 k1) 1

(** Returns set of interval (k1, k2), left son l and right son r. 
    Assumes that:
    k1 <= k2
    all elements of l are lesser than k1
    all emements of r are bigger than k2 *)
let make l (k1, k2) r = 
  Node (l, (k1, k2), r, max (height l) (height r) + 1, (plus (plus_count l r) (length (k1, k2))))

(** Returns balanced set of interval (k1, k2), left son l and right son r. 
    Assumes that:
    k1 <= k2
    all elements of l are lesser than k1
    all emements of r are bigger than k2 
    |height l - height r| <= 3*)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
         | Node (lrl, lrk, lrr, _, _) ->
           make (make ll lk lrl) lrk (make lrr k r)
         | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
         | Node (rll, rlk, rlr, _, _) ->
           make (make l k rll) rlk (make rlr rk rr)
         | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, (plus (plus_count l r) (length k)))

(** Returns interval with smallest elements of the given set *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(** Returns interval with biggest elements of the given set *)  
let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

(** Returns given set without interval with smallest elements *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(** Returns given set without interval with biggest elements *)
let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "ISet.remove_max_elt"

let cmp = compare

(** Returns a set containing the same elements as s,
    plus all elements of the interval (x1, x2)
    Assumes x1 <= x2 and s doesn't contain any element of (x1, x2) *)      
let rec add_one (x1, x2) s =
  match s with
  | Node (l, k, r, h, i) ->
    let c = cmp (x1, x2) k in
    if c = 0 then Node (l, (x1, x2), r, h, i)
    else if c < 0 then
      let nl = add_one (x1, x2) l in
      bal nl k r
    else
      let nr = add_one (x1, x2) r in
      bal l k nr
  | Empty -> Node (Empty, (x1, x2), Empty, 1, (length (x1, x2)))

(** Returns set of interval (v1, v2), left son l and right son r.
    Assumes:
    v1 <= v2
    all elements of l are lesser than v1
    all elements of r are bigger than v2 *)
let rec join l (v1, v2) r =
  match (l, r) with
    (Empty, _) -> add_one (v1, v2) r
  | (_, Empty) -> add_one (v1, v2) l
  | (Node(ll, lv, lr, lh, lc), Node(rl, rv, rr, rh, rc)) ->
    if lh > rh + 2 then bal ll lv (join lr (v1, v2) r) else
    if rh > lh + 2 then bal (join l (v1, v2) rl) rv rr else
      make l (v1, v2) r

(** Returns a triple (l, present, r), where
    l is the set of elements of s that are strictly lesser than x;
    r is the set of elements of s that are strictly greater than x;
    present is false if s contains no element equal to x,
    or true if s contains an element equal to x. *)   
let rec split x s =
  match s with
    Empty ->
    (Empty, false, Empty)
  | Node (l, v, r, _, _) ->
    match v with (v1, v2) ->
      if v1 = x && v2 = x then (l, true, r)
      else if v1 = x then (l, true, (add_one (x+1, v2) r))
      else if v2 = x then ((add_one (v1, x-1) l), true, r)
      else if x < v1 then let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
      else if x > v2 then let (lr, pres, rr) = split x r in (join l v lr, pres, rr)
      else ((add_one (v1, x-1) l), true, (add_one (x+1, v2) r))

(** Returns true if set contains x, and false otherwise. *)            
let rec mem x set =
  match set with
  | Empty -> false
  | Node (l, k, r, _, _) ->
    match k with (k1, k2) ->
      if x >= k1 && x <= k2 then true
      else if x < k1 then mem x l
      else mem x r

(** Returns a set containing the same elements as s,
    except for all those which are included in interval (x1, x2)
    Assumes x1 <= x2 *)
let remove (x1, x2) s =
    let (l, _, _) = split x1 s in
    let (_, _, r) = split x2 s in
    match r with
    | Empty -> l;
    | _ -> join l (min_elt r) (remove_min_elt r) 

(** Applies f to all continuous intervals in the set s.
    The intervals are passed to f in increasing order. *)   
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop s

(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
      loop (f k (loop acc l)) r in
  loop acc s 

(** Returns the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order. *)   
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(** Returns the number of elements of set that are lesser
    or equal to x. If there are more than max_int such elements, 
    the result should be max_int. *)  
let below x set =
  match split x set with (l, b, _) ->
    if b then plus (count l) 1
    else (count l)

(** Returns pair of interval (x1, x2) merged with 
    neighbouring interval (l1, l2) in set l (if it exists)
    and set l without element (l1, l2) 
    Assumes (x1, x2) > max_elt l*)
let ext_left (x1, x2) l =
  match l with 
  | Empty -> ((x1, x2), Empty)
  | Node (_, _, _, _, _) ->
    match max_elt l with (l1, l2) ->
      if l2 = x1 - 1 then ( (l1, x2), remove_max_elt l)
      else ((x1, x2), l)

(** Returns pair of interval (x1, x2) merged with
    neighbouring interval (r1, r2) in set r (if it exists)
    and set r without element (r1, r2) 
    Assumes (x1, x2) < min_elt r*)      
let ext_right (x1, x2) r=
  match r with 
  | Empty -> ((x1, x2), Empty)
  | Node (_, _, _, _, _) ->
    match min_elt r with (r1, r2) ->
      if r1 = x2 + 1 then ( (x1, r2), remove_min_elt r)
      else ((x1, x2), r)

(** Returns a set containing the same elements as s,
    plus all elements of the interval (x1, x2) including x1 and x2.
    Assumes x1 <= x2. *)      
let add (x1, x2) s =
  let (l, _, _) = split x1 s in
  let (_, _, r) = split x2 s in
  let (new_x, new_l) = ext_left (x1, x2) l in
  let (new_new_x, new_r) = ext_right new_x r in
  join new_l new_new_x new_r

(* TESTY *) (*
let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;


let s = empty;;
test 11 (is_empty s) true;;
test 12 (is_empty (add (1, 1) s)) false;;


(* niestety musimy zalozyc poprawnosc mem... *)

let s = add (10, 12) empty;;
test 21 (mem 9 s) false;;
test 22 (mem 10 s) true;;
test 23 (mem 12 s) true;;
test 24 (mem 13 s) false;;

let s = add (4, 7) s;;
test 25 (mem 8 s) false;;
test 26 (mem 11 s) true;;
test 27 (mem 5 s) true;;
test 28 (mem 3 s) false;;


let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 31 (mem 10 (remove (10, 10) s)) false;;
test 32 (is_empty (remove (1, 20) s)) true;;
test 33 (mem 7 (remove (8, 15) s)) true;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 34 (is_empty s) false;;
test 35 (mem 5 s) false;;
test 36 (mem (-10) s) false;;
test 37 (mem (-15) s) true;;
test 38 (mem 17 s) true;;


test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
let l, pres, r = split 9 s;;
test 61 (mem 9 l) false;;
test 62 (mem 9 r) false;;
test 63 (mem 8 l) true;;
test 64 (mem 10 r) true;;
test 65 pres true;;
test 66 (mem 7 l) false;;
test 67 (mem 4 l) true;;
test 68 (mem 11 r) false;;
test 69 (mem 16 r) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let a = ref [];;
let foo x = a := x::!a; ();;
test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let foo x a = x::a;;
test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;;


let _ =
    if !zle = 0 then
        ()
    else
        Printf.printf "\nZlych odpowiedzi: %d.\n" !zle
;; *)
