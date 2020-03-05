(* Autor : Szymon Borowy *)
(* Przelewanka *)

(* usuwamy pary postaci (0,0) *)
let usuwamyzera szklanki =
   let zera acc (a,b) = 
      if a<>0 then (a,b)::acc
      else acc in
   let lista = Array.fold_left zera [] szklanki in
   Array.of_list (List.rev lista);;

   
(* funkcja pomocnicza *)   
let nwd x y = 
   let rec pom a b =
      if b=0 then a
      else pom b (a mod b) in
   if x>y then pom x y else pom y x;;
   
   
(* koncowe ilosci wody podzielne przez nwd pojemnosci*)   
let nwdpojemnosci szklanki =
   let dzielnik = Array.fold_left (fun acc (a,b) -> nwd acc a) 0 szklanki in
   let p (a,b) =
      (b mod dzielnik)=0 in
   Array.for_all p szklanki;;

   
(* 1 szklanka musi byc pusta lub pelna na koncu *)   
let pustapelna szklanki = 
   let p (a,b) =
      (a = b) || (b = 0) in
   Array.exists p szklanki;;

   
(* przypadek, gdy wszystkie koncowo maja byc pelne *)   
let wszystkiepelne szklanki = 
   let p (a,b) = 
      a = b in
   Array.for_all p szklanki;;
   
   
(* sprawdza czy stan odpowiada specyfikacji wyniku *)
let czywynik szklanki stan =
   let dlugosc = Array.length szklanki in
   let koncowe = Array.init dlugosc (fun i -> (snd szklanki.(i)) ) in
   koncowe = stan;;
 
 
(* przejscie z obecnego stanu do nastepnego poprzez
   jedna z dozwolonych czynnosci*)
let nowestany szklanki mapa kolejka (stan, kroki) wynik = 
   let dlugosc = Array.length szklanki in
   (* nalewamy do pelna wody do ktorejs ze szklanek *)
   for i = 0 to dlugosc-1 do 
      let kopia = Array.copy stan in
      kopia.(i)<-(fst szklanki.(i));
      (* wrzucamy na mape, jak nie bylo jeszcze takiego stanu *)
      if not(Hashtbl.mem mapa kopia) then begin
         Hashtbl.add mapa kopia 1;
         Queue.add (kopia, kroki+1) kolejka;
         if (czywynik szklanki kopia) then wynik:=kroki+1;
         end
     done;
  (* wylewamy cala wode z ktorejs ze szklanek *)
  for i = 0 to dlugosc-1 do
     let kopia = Array.copy stan in
     kopia.(i)<-0;
     (* wrzucamy na mape, jak nie bylo jeszcze takiego stanu *)
     if not(Hashtbl.mem mapa kopia) then begin
        Hashtbl.add mapa kopia 1;
        Queue.add (kopia, kroki+1) kolejka;
        if (czywynik szklanki kopia) then wynik:=kroki+1;
        end
    done;
  (* przelewamy wode z jednej szklanki do drugiej *)
  for i = 0 to dlugosc-1 do
     for j = 0 to dlugosc-1 do
        if i<>j && stan.(i)<>0 && stan.(j)<>(fst szklanki.(j)) then
           let kopia = Array.copy stan in
           let skladnik = min kopia.(i) (-kopia.(j)+(fst szklanki.(j))) in
           begin
           kopia.(j)<-(kopia.(j)+skladnik);
           kopia.(i)<-(kopia.(i)-skladnik);
           (* wrzucamy na mape, jak nie bylo jeszcze takiego stanu *)
           if not(Hashtbl.mem mapa kopia) then begin
              Hashtbl.add mapa kopia 1;
              Queue.add (kopia, kroki+1) kolejka;
              if (czywynik szklanki kopia) then wynik:=kroki+1;
           end
        end
        done
     done;;

     
let przelewanka x = 
   (* nowa tablica bez par (0,0) *)
   let szklanki = usuwamyzera x in  
   let dlugosc = Array.length szklanki in
   (* pusta tablica -> wynik 0 *)
   if Array.length szklanki = 0 then 0
   (* wykluczamy oczywiste przypadki, gdy sie nie da *)
   else if not(nwdpojemnosci szklanki && pustapelna szklanki) then -1
   else if wszystkiepelne szklanki then
       Array.fold_left(fun acc (a,b) -> if b<>0 then acc+1 else acc) 0 szklanki
   (* przechodzimy kolejne stany generujac kolejne az osiagniemy wynik lub kolejka pusta *)
   else
      let mapa = Hashtbl.create 0 in
      let kolejka = Queue.create() in
      (* stan poczatkowy - wszystkie szklanki puste *)
      let pocz = Array.make dlugosc 0 in
      let wynik = ref (-1) in
      begin
      Hashtbl.add mapa pocz 1;
      Queue.add (pocz, 0) kolejka; 
      (* jezeli poczatkowy stan to szukany wynik *)
      if (czywynik szklanki pocz) then wynik:=!wynik+1;
      (* dopoki nie mamy wyniku lub na kolejce sa jakies stany *)
      (* obrabiamy pary (gora,kroki), gdzie gora to dany stan,
         a kroki to liczba przeksztalcen od stanu poczatkowego *)
      while (not(Queue.is_empty kolejka) && !wynik=(-1)) do
         let (gora, kroki) = Queue.take kolejka in
         nowestany szklanki mapa kolejka (gora, kroki) wynik;
         done
      end;
      !wynik;
      ;;
