(* leftist *)
(* Autor : Szymon Borowy *)

(* Node - wezel skladajacy sie z priorytetu, skrajnie prawej wysokosci drzewa,
          lewego poddrzewa i prawego poddrzewa
   Leaf - puste drzewo *)
type 'a queue = 
   Leaf | Node of 'a * int * 'a queue * 'a queue;;


let empty = 
   Leaf;;

(* zwraca skrajnie prawa wysokosc drzewa w kolejce *)
let height q =
   match q with
      Leaf -> 0 |
      Node(p, h, l, r) -> h;;
      
(* laczenie kolejek *)      
let rec join q1 q2 =
   match (q1, q2) with
      (Leaf, Leaf) -> Leaf |
      (Leaf, _) -> q2 |
      (_, Leaf) -> q1 |
      (Node(p1, h1, l1, r1), Node(p2, h2,  l2, r2)) ->
         if p1 <= p2 then 
            let d3 = join r1 q2
            in if height d3 < height l1 then Node(p1, height d3 + 1, l1, d3)
               else Node(p1, height l1 + 1, d3, l1)
         else 
            join q2 q1;;

(* laczenie kolejki jednoelementowej i q *)
let add e q = 
   join (Node(e, 0, Leaf, Leaf)) q;;


exception Empty;;

(* usuwanie korzenia kolejki i laczenie poddrzew *)
let delete_min q = 
   match q with
      Leaf -> raise Empty |
      Node(priority, height, left, right) -> (priority, join left right);;

   
   
let is_empty q = 
   if q = Leaf then true
   else false;;
         
      

