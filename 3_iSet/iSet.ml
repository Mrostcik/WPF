(* Interval Set *)
(* Autor : Szymon Borowy *)


(*drzewo = zbior, którego elementami są przedzialy, przechowuje dodatkowo wysokosc i liczbe elementow w drzewie  *)
type t =
   Empty |
   Node of t * (int * int) * t * int * int;;
 
 
(*tworzymy pusty zbior *)  
let empty = 
   Empty;;

   
(* sprawdzamy, czy zbior jest pusty *)   
let is_empty s = 
   s = Empty;;

   
(* wyluskuje wysokosc poddrzewa *)
let height s = 
   match s with
      | Node (_, _, _, h, _) -> h
      | Empty -> 0;;

      
(* wyluskuje ilosc elementow  w poddrzewie *)
let quantity s = 
   match s with
      | Node(_,_,_,_, q) -> q
      | Empty -> 0;;
      
      
(* tworzymy drzewo z lewym poddrzewem l, korzeniem (x,y) i prawym poddrzewem r *)
let make l (x,y) r = 
   let lq = quantity l in
   let rq = quantity r in
   (* jezeli liczba elementow w poddrzewie przekracza max_int to zwracamy max_int *)
   let w =
   (* dwa przypadki, gdy w ktoryms z poddrzew juz jest max_int elementow *)
   if lq = max_int then max_int 
   else if rq = max_int then max_int
   (* jako ze liczba elementow w poddrzewie moze byc maksymalnie max_int to sprawdzamy, 
      czy po dodaniu jakiejs liczby elementow przekraczamy max_int *) 
   else if lq+rq< lq || lq+rq <rq then max_int 
   else if lq+rq+y-x+1<lq+rq || lq+rq+y-x+1<y-x+1 then max_int
   (* nie przekroczylismy max_int, zwracamy sume *)
   else lq+rq+y-x+1
   in
   Node (l, (x,y) , r, max (height l) (height r) + 1, w )

   
(* balansowanie drzewa - funkcja wzieta z biblioteki Polymphorphic Sets, utrzymuje wywazenie drzewa *)
let bal l k r =
   let hl = height l in
   let hr = height r in
   (* sprawdzamy wysokosc, ktorego poddrzewa jest wieksza *)
   if hl > hr + 2 then
     match l with
     | Node (ll, lk, lr, _, _) ->
         (* balansujemy *)
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
         (* balansujemy *)
         if height rr >= height rl then make (make l k rl) rk rr
         else
           (match rl with
           | Node (rll, rlk, rlr, _, _) ->
               make (make l k rll) rlk (make rlr rk rr)
           | Empty -> assert false)
     | Empty -> assert false
   else make l k r;; 
 

(* szukanie minimalnego elementu w drzewie *)
let rec min_elt = function
   | Node (Empty, k, _, _, _) -> k
   (* idziemy do skrajnie lewego liscia *)
   | Node (l, _, _, _, _) -> min_elt l
   | Empty -> raise Not_found


(* usuwanie minimalnego elementu z drzewa *)
let rec remove_min_elt = function
   | Node (Empty, _, r, _, _) -> r
   (* idziemy do skajnie lewego liscia i go usuwamy *)
   | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
   | Empty -> invalid_arg "PSet.remove_min_elt"

   
(* szukanie maksymalnego elementu w drzewie *)   
let rec max_elt = function
   | Node(_, k, Empty, _, _) -> k
   (* idziemy do skrajnie prawego liscia *)
   | Node(_, _, r, _, _) -> max_elt r
   | Empty -> raise Not_found

   
(* scalanie dwoch drzew *)
(* t1<t2 *)
let merge t1 t2 =
   match t1, t2 with
   | Empty, _ -> t2
   | _, Empty -> t1
   | _ ->
       (* przestawiamy minimalny element z t2 na korzen nowego drzewa *)
       let k = min_elt t2 in
       bal t1 k (remove_min_elt t2)

       
(* laczenie dwoch poddrzew l i r, gdzie ich korzeniem staje sie (x,y) *)      
(* l<(x,y)<r *)
let rec join l (x,y) r =
  match (l, r) with
    (Empty, _) -> make empty (x,y) r
  | (_, Empty) -> make l (x,y) empty 
  | (Node(ll, (x1,y1)  , lr, lh, _), Node(rl, (x2,y2) , rr, rh, _)) ->
      (* utrzymujemy wywazenie drzewa *)   
      if lh > rh + 2 then bal ll (x1,y1) (join lr (x,y) r) else
      if rh > lh + 2 then bal (join l (x,y) rl) (x2,y2) rr else
      make l (x,y) r;;
  
  
(* zwraca trojke (drzewo elementow mniejszych od x, 
                  true/ false zaleznie czy x jest w drzewie,
                  drzewo elementow wiekszych od x) *)  
let rec split x s = 
   match s with
      Empty -> 
         (Empty, false, Empty) |
      Node ( l, (p,k), r, _, _) -> 
         (* czy x zawiera sie danym przedziale *)
         if (x<k && x>p) then (join l (p, x-1) empty, true, join empty (x+1,k) r)
         (* czy x jest koncem i poczatkiem danego przedzialu *)
         else if x=p && x=k then (l, true, r)
         (* czy x jest poczatkiem danego przedzialu *)
         else if x=p then (l , true, join Empty (x+1,k) r)
         (* czy x jest koncem danego przedzialu *)
         else if x=k then (join l (p,x-1) empty , true, r)
         (* czy x jest mniejsze od danego przedzialu *)
         else if x<p then 
         let (ll, pres, rl) = split  x l in (ll, pres, join rl (p,k) r )
         (* x jest wieksze od danego przedzialu *)
         else 
         let (lr, pres, rr) = split x r in (join l (p,k) lr, pres, rr);;
   
   
(* usuwamy element (x,y) z drzewa *)      
let remove (x,y) s = 
   let (l, _, _) = split x s in
   let (_, _, r) = split y s in
   (* laczymy drzewo elementow mniejszych od x i drzewo elementow wiekszych od y *)
   merge l r;;

(* wyluskuje lewe poddrzewo *)
let left s =
   match s with
      Empty -> Empty |
      Node (ll, _ , _, _, _) ->ll;;
      
      
(* wyluskuje prawe poddrzewo *)      
let right  s =
   match s with
      Empty -> Empty |
      Node (_, _ , rr, _, _) ->rr;;
 
 
(* wyluskuje przedzial z wezla *) 
let value s = 
   match s with
      Empty -> (0,0) |
      Node (_, (x,y) , _, _, _) -> (x,y) ;;
      
      
(* dodawanie przedzialu (x,y) do drzewa *)      
let add (x,y) s =
   (* tworzymy drzewo elementow mniejszych od x i drzewo wiekszych od y *)
   let (l, _, _) = split x s in
   let (_, _, r) = split y s in
   (* szukamy maksymalnych elementow w l i r, jezeli to mozliwe, przyda sie w tej funkcji *)
   let (vl1,vl2) = 
      if l<>empty then max_elt l
      else (x,y) 
   in
   let (vr1,vr2) =
      if r<>empty then min_elt r 
      else (x,y)
   in
   let (l1, _, r1) = split vl1 l in
   let (l2, _, r2) = split vr2 r in
   (* sprawdzamy, czy (x,y) mozna skleic z przedzialami z l i r *)
   if (vl2 = x-1 && vr1 = y+1) then bal l1 (vl1,vr2) r2
   (* czy (x,y) mozna skleic z przedzialem z l *)
   else if vl2 = x-1 then bal l1 (vl1,y) r
   (* czy (x,y) mozna skleic z przedzialem z r *)
   else if vr1 = y+1 then bal l (x,vr2) r2
   (* nie mozna skleic *)
   else bal l (x,y) r;;


(* sprawdzamy, czy dany element jest w drzewie *)   
let mem x s = 
   (* zwracamy druga wartosc z funkcji split *)
   let (_, is, _) = split x s in
   is = true;;
   

(* podajemy do funkcji jako argumenty elementy drzewa po kolei od najmniejszego *)  
let rec iter f s =
   match s with
      Empty -> () |
      Node (l, (x,y) , r, _, _) ->  iter f l; f (x,y); iter f r;;


(* podajemy do funkcji jako argumenty elementy drzewa po kolei od najmniejszego,
   akumulujemy wynik *)  
let rec fold f s a =
   let rec loop acc = function
      Empty -> acc |
      Node(l, (x,y), r, _, _) ->  loop (f (x,y) (loop acc l)) r in
   loop a s ;;


(* tworzymy liste elementow znajdujacych sie w drzewie *)   
let elements s =
   let rec loop acc = function
      Empty -> acc |
      Node (l, (x,y), r, _, _) -> loop ((x,y) :: loop acc r) l in
    loop [] s;;

    
(* zwracamy liczbe elementow w drzewie <= od n  *)    
let below n s =
   let (l, is, _) = split n s in
   (* jezeli element znajduje sie w drzewie i nie przekroczylismy max_int *)
   if is=true && (quantity l) + 1 > quantity l then (quantity l)+1
   (* jezeli znajduje sie i przekroczylismy max_int *)
   else if is = true then max_int
   (* nie znajduje sie *)
   else quantity l;;
   

   
   

