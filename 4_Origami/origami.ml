(* Origami *)
(* Autor: Szymon Borowy *)

(* epsilon, aby przeciwstawic sie niedokladnosci floatow przy dzieleniu *)
let eps = epsilon_float *. 1000000.;;


(* punkt - wspolrzedne (x,y) *)
type point = float * float;;


(* kartka - funkcja zwracajaca ile razy kartke przebije szpilka w danym punkcie *)
type kartka = point -> int;;


(* informacja po ktorej stronie danej prostej znajduje sie punkt *)
type strony =  Lewa | Na | Prawa;;


(* kartka - prostokat *)
let prostokat (lx, ly) (rx, ry) (x, y) =
   if eps+.x>=lx && x<=rx+.eps && eps+.y>=ly && y<=ry+.eps then 1
   else 0;;
   
   
(* kartka - kolko *)   
let kolko (mx, my) r (x, y) =
   if (sqrt((x-.mx)*.(x-.mx) +. (y-.my)*.(y-.my))) <= r +. eps then 1 
   else 0;;
   
   
(* liczymy po ktorej stronie prostej(wektora) znajduje sie punkt *)   
let po_ktorej_stronie (px1, py1) (px2, py2) (x, y) = 
   (* obliczamy iloczyn wektorowy *)
   let iloczyn = (px2-.px1)*.(y-.py1) -. (x-.px1)*.(py2-.py1) in
   if iloczyn<=0.+.eps && iloczyn>=0.-.eps then Na
   else if iloczyn>0. then Lewa
   else Prawa;;

   
(* skladamy kartke wzdluz danej prostej *)
(* przebicie po prawej stronie prostej wynosi 0 *)
(* przebicie na prostej wynosi tyle co przed zlozeniem *)
(* przebicie po lewej stronie wynosi tyle co przed zlozeniem + 
   przebicie rozlozonej kartki w punkcie, ktory nalozyl sie na punkt przebicia
   (czyli przebicie punktu symetrycznego do danego) *)
let zloz (px1, py1) (px2, py2) k (x, y) = 
   (* przypadek, gdy prosta rownolegla do osi OY *)
   if px1=px2 then begin
      (* po ktorej stronie prostej lezy punkt *)
      let strona =
         if x<=px1+.eps && x>=px1-.eps then Na
         else if x<px1 && py2>py1 then Lewa
         else if x<px1 && py2<=py1 then Prawa
         else if x>px1 && py2>py1 then Prawa
         else Lewa in
      (* zwracamy liczbe przebic w danym punkcie *)
      if strona = Prawa then 0
      else if strona = Na then k (x,y)
      else k (x,y) + k (2.*.px1-.x, y)
      end
   (* przypadek, gdy prosta rownolegla do osi OX *)
   else if py1=py2 then begin
      (* po ktorej stronie prostej lezy punkt *)
      let strona = 
         if y<=py1+.eps && y>=py1-.eps then Na
         else if y>py1 && px1<=px2 then Lewa
         else if y>py1 && px1>px2 then Prawa
         else if y<py1 && px1<px2 then Prawa
         else Lewa in
      (* zwracamy liczbe przebic w danym punkcie *)
      if strona = Prawa then 0
      else if strona = Na then k (x,y)
      else k (x,y) + k (x, 2.*.py1-.y)
      end   
   (* pozostale przypadki *)
   else begin
   let strona = po_ktorej_stronie (px1, py1) (px2, py2) (x,y) in
   if 
      strona = Prawa then 0
   else if 
      strona = Na then k (x, y)
   else begin
      (* obliczamy wspolrzedne punktu symetrycznego do danego *)
      (* rownanie prostej *)
      let wspolczynnik1 = (py2-.py1)/.(px2-.px1) in
      let b1 = py2-.(wspolczynnik1*.px2) in
      (* rownanie prostej prostopadlej *)
      let wspolczynnik2 = (px1-.px2)/.(py2-.py1) in
      let b2 = y-.(wspolczynnik2*.x) in
      (* wspolrzedne punktu przeciecia prostej i prostej prostopadlej *)
      let przecieciex = (b2-.b1)/.(wspolczynnik1-.wspolczynnik2) in
      let przecieciey = (wspolczynnik2*.przecieciex)+.b2 in
      (* wspolrzedne punktu symetrycznego *)
      let symx = (2.*.przecieciex)-.x in
      let symy = (2.*.przecieciey)-.y in
      k (x,y) + k (symx, symy)
      end
      end;;
      
(* zlozenie kartki wzdluz kolejnych prostych z listy *)      
let skladaj l k =
   let f acc ((p1,p2) ,(p3,p4)) =
      zloz (p1,p2) (p3,p4) acc in
   List.fold_left f k l;;
