(* Sortowanie topologiczne *)
(* Autor: Szymon Borowy *)
  
exception Cykliczne;;

let licz = ref 0 ;;
let mapa = Hashtbl.create 0;;
let wychodzace = Array.make 100 0;;
let tab = Array.make 100 [];;
let wynik = ref [];;
let rec przechodzimy l =
   match l with
      (a,b)::t -> if not(Hashtbl.mem mapa a ) then 
       begin 
       Hashtbl.add mapa a !licz;
       licz:=!licz+1;
       przechodzimy t
       end
       | [] -> ();;
    
       
let mapowanie l = 
   let rec przechodzimy3 b =
      match b with
         h2::t2 -> if not(Hashtbl.mem mapa h2 ) then
         begin
         Hashtbl.add mapa h2 !licz;
         licz:=!licz+1;
         przechodzimy3 t2
         end 
         | [] -> () in 
   let rec przechodzimy2 l =
      match l with
         (a,b)::t -> tab.(Hashtbl.find mapa a)<- b;
                     przechodzimy3 b;
                     przechodzimy2 t
         | [] -> ()            
                     in
   przechodzimy2 l;;

let kolejka = Queue.create();;
let rec dodajemy l2 = 
      match l2 with
         h2::t2 ->let etykieta = Hashtbl.find mapa h2 in
                  wychodzace.(etykieta)<- (wychodzace.(etykieta)+1);
                  dodajemy t2 
         |[] -> ();;
         
let rec ilewychodzacych l =
   match l with
      (a,b)::t -> dodajemy b;
                  ilewychodzacych t
      | [] -> ();;            


let rec czymozna l = 
   match l with
      | (a,b)::t -> let et = Hashtbl.find mapa a in
                    if wychodzace.(et)=0 then begin
                                              Queue.add et kolejka;
                                              wynik:=a::!wynik;
                                              end;
                    czymozna t;
      | [] -> ();;

let rec topolpom l = 
   match l with
      h::t -> 
              let et = Hashtbl.find mapa h in
              wychodzace.(et)<- (wychodzace.(et)-1);
              if wychodzace.(et)=0 then begin
                                           Queue.add et kolejka;
                                           wynik:=h::!wynik;
                                           end;
             topolpom t;
      | [] -> ();;
      
let topol l=
   przechodzimy l;
   let licz2 = ref !licz in
   mapowanie l;
   ilewychodzacych l;
   czymozna l;
   while (not(Queue.is_empty kolejka)) do
      let gora = Queue.take kolejka in
      if gora < !licz2 then 
                               begin 
                               let sasiedzi = tab.(gora) in
                               topolpom sasiedzi;
                               end;
      done;
   if List.length !wynik = !licz then !wynik
   else raise Cykliczne;
