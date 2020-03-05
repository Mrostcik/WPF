(* Autor : Szymon Borowy  *)


type wartosc = (* gdy rozlaczne: true to mamy (-inf,najmniejsza> U <najwieksza, inf),
                w przeciwnym wypadku mamy <najmniejsza,najwieksza>*)
{ najmniejsza:float; najwieksza:float; rozlaczne:bool};; 

let rec sumazbiorow a b =  (*suma zbiorów wykorzystywana potem w mnożeniu i dzieleniu*)
   if a.rozlaczne=false && b.rozlaczne=false && a.najmniejsza>b.najmniejsza then 
      sumazbiorow b a
   (* oba zbiory nierozłączne i b zawiera się w a *)
   else if a.rozlaczne=false && b.rozlaczne=false && 
   a.najmniejsza<=b.najmniejsza && a.najwieksza>=b.najwieksza then 
      {najmniejsza = a.najmniejsza; najwieksza = a.najwieksza; rozlaczne = false}
   (* oba zbiory nierozłączne i nachodzą na siebie *)
   else if a.rozlaczne=false && b.rozlaczne=false && 
   a.najmniejsza<=b.najmniejsza && a.najwieksza>=b.najmniejsza then 
      {najmniejsza = a.najmniejsza; najwieksza = b.najwieksza; rozlaczne = false}
   (* oba zbiory nierozłączne postaci (-inf,x> i <y,inf) *)
   else if a.rozlaczne=false && b.rozlaczne=false &&
   a.najmniejsza<=b.najmniejsza && a.najwieksza<b.najmniejsza then 
      {najmniejsza = a.najwieksza; najwieksza = b.najmniejsza ; rozlaczne= true} 
   (* jeden zbiór rozłączny, a drugi nierozłączny i drugi zawiera się w pierwszym składniku pierwszego zbioru *)
   else if a.rozlaczne=true && b.rozlaczne = false &&
   b.najwieksza<=a.najmniejsza then 
      {najmniejsza = a.najmniejsza ; najwieksza = a.najwieksza; rozlaczne = true}
   (* jeden zbiór rozłączny, a drugi nierozłączny i drugi zawiera się w drugim składniku pierwszego zbioru *)
   else if a.rozlaczne = true && b.rozlaczne=false &&
   b.najmniejsza>=a.najwieksza then 
      {najmniejsza = a.najmniejsza; najwieksza = a.najwieksza; rozlaczne = true}
   (* jeden zbiór rozłączny, a drugi nierozłączny - pozostałe przypadki *)
   else if a.rozlaczne=true && b.rozlaczne=false then 
      sumazbiorow (sumazbiorow ({najmniejsza=neg_infinity; najwieksza = a.najmniejsza; rozlaczne=false}) b) 
                  (sumazbiorow ({najmniejsza=a.najwieksza; najwieksza=infinity;rozlaczne=false}) b)
   else if a.rozlaczne=false && b.rozlaczne=true then 
      sumazbiorow b a
   (* oba zbiory rozłączne *)
   else 
      sumazbiorow ({najmniejsza = neg_infinity; najwieksza = max a.najmniejsza b.najmniejsza; rozlaczne= false}) 
                  ({najmniejsza = min a.najwieksza b.najwieksza; najwieksza = infinity; rozlaczne=false});;

let wartosc_od_do x y =
   {najmniejsza = x; najwieksza = y; rozlaczne = false};;

let wartosc_dokladnosc x p =
   {najmniejsza = min (x *. (1.0 -. (p /. 100.0)))  (x *. (1.0 +. (p /. 100.0))); najwieksza = max (x *. (1.0 -. (p /. 100.0)))  (x *. (1.0 +. (p /. 100.0))); rozlaczne = false};;

let wartosc_dokladna x = 
   wartosc_od_do x x;;

let in_wartosc x y = 
   if x.rozlaczne=false && y >= x.najmniejsza && y<= x.najwieksza then
      true 
   else if x.rozlaczne=true && (y<=x.najmniejsza || y>=x.najwieksza) then
      true
   else 
      false;;

let min_wartosc x = 
   if x.rozlaczne=false then 
      x.najmniejsza
   else 
      neg_infinity;;

let max_wartosc x = 
   if x.rozlaczne=false then 
      x.najwieksza
   else
      infinity;;

let sr_wartosc x = 
   (min_wartosc x +. max_wartosc x) /. 2.0;;

let rec plus a b = 
   (* a lub b - zbiór pusty *)
   if (classify_float b.najmniejsza = FP_nan) || (classify_float a.najmniejsza = FP_nan) then 
      {najmniejsza = nan; najwieksza = nan; rozlaczne = false}
   (* oba zbiory nierozłączne *)
   else if a.rozlaczne=false && b.rozlaczne=false then
      {najmniejsza = a.najmniejsza +. b.najmniejsza; najwieksza = a.najwieksza +. b.najwieksza; rozlaczne = false}
   (* jeden zbiór rozłączny, a drugi nierozłączny *)
   else if a.rozlaczne=true && b.rozlaczne = false then 
      sumazbiorow (plus {najmniejsza = neg_infinity; najwieksza = a.najmniejsza; rozlaczne= false} b)
                  (plus {najmniejsza=a.najwieksza; najwieksza=infinity; rozlaczne=false} b)
   else if a.rozlaczne=false && b.rozlaczne = true then
      plus b a
   (* oba zbiory rozłączne *)
   else 
      {najmniejsza = neg_infinity; najwieksza = infinity; rozlaczne = false};;

let minus a b = (*dodawanie zbioru przeciwnego*)
   (* zbiór, który odejmujemy nierozłączny*)
   if b.rozlaczne=false then 
      plus a {najmniejsza = -1.0 *. b.najwieksza; najwieksza = -1.0 *. b.najmniejsza; rozlaczne = false}
   (* zbiór, który odejmujemy rozłączny*)
   else 
      plus a {najmniejsza = -1.0 *. b.najwieksza; najwieksza = -1.0 *. b.najmniejsza; rozlaczne = true};;
 
let mnozenie a b = (* aby nie psuło się z powodu tego, że w ocamlu 0 nie równa się -0 , używane w procedurze razy zamiast *. *)
   if a=0.0 || b=0.0 ||a= -0.0 || b= -0.0 then
      0.0
   else 
      a*.b;;
  

let rec razy a b =
   (* a lub b - zbióry pusty *)
   if (classify_float b.najmniejsza = FP_nan) || (classify_float a.najmniejsza = FP_nan) then 
      {najmniejsza = nan; najwieksza = nan; rozlaczne = false}
   (* oba zbiory nierozłączne *)
   else if a.rozlaczne=false&&b.rozlaczne=false then 
      {najmniejsza = min (min (mnozenie a.najmniejsza  b.najwieksza) (mnozenie a.najwieksza b.najwieksza))
                         (min (mnozenie a.najwieksza  b.najmniejsza) (mnozenie a.najmniejsza  b.najmniejsza));
      najwieksza = max (max (mnozenie a.najmniejsza b.najwieksza) (mnozenie a.najwieksza  b.najwieksza)) 
                       (max (mnozenie a.najwieksza b.najmniejsza) (mnozenie a.najmniejsza  b.najmniejsza));
      rozlaczne = false}
   (* jeden zbióry rozłączny, a drugi nierozłączny *)
   else if a.rozlaczne = true && b.rozlaczne = false then
      sumazbiorow (razy ({najmniejsza = neg_infinity; najwieksza = a.najmniejsza; rozlaczne = false}) b ) 
                  (razy ({najmniejsza = a.najwieksza; najwieksza = infinity; rozlaczne = false}) b )
   else if a.rozlaczne = false && b.rozlaczne = true then
      razy b a
   (* oba zbiory rozłączne *)
   else
      sumazbiorow (razy a ({najmniejsza = neg_infinity; najwieksza = b.najmniejsza; rozlaczne = false}) ) 
                  (razy a ({najmniejsza = b.najwieksza; najwieksza = infinity; rozlaczne = false}));;

let rec podzielic a b = (* mnożenie przez zbiór odwrotny *)
   (* nie dzielimy przez zero i zwracamy zbiór pusty *)
   if b.najmniejsza=0.0 && b.najwieksza=0.0 && b.rozlaczne=false then 
      {najmniejsza = nan; najwieksza = nan; rozlaczne = false}
   (* a jest zbiorem pustym, więc otrzymujemy zbiór pusty *)
   else if (classify_float a.najmniejsza = FP_nan) || (classify_float b.najmniejsza = FP_nan) then 
      {najmniejsza = nan; najwieksza = nan; rozlaczne = false}
   (* b postaci <x,0> *)
   else if b.najwieksza=0.0 && b.rozlaczne = false then 
      razy a {najmniejsza = neg_infinity; najwieksza = (1.0 /. b.najmniejsza); rozlaczne= false}
   (* b postaci <0,x> *)
   else if b.najmniejsza=0.0 && b.rozlaczne = false then 
      razy a {najmniejsza = (1.0 /. b.najwieksza); najwieksza = infinity; rozlaczne = false}
   (* b postaci <-x,-y> lub <x,y> , gdzie x,y>0 *)
   else if b.najmniejsza *. b.najwieksza >= 0.0 && b.rozlaczne = false then 
      razy a {najmniejsza = min (1.0 /. b.najmniejsza) (1.0 /. b.najwieksza) ; najwieksza = max (1.0 /. b.najmniejsza) (1.0 /. b.najwieksza); rozlaczne = false}
   (* b postaci <-x,y> , gdzie x,y>0 *)
   else if b.najmniejsza *. b.najwieksza <0.0 && b.rozlaczne = false then 
      razy a { najmniejsza = min (1.0 /. b.najmniejsza) (1.0 /. b.najwieksza) ; najwieksza = max (1.0 /. b.najmniejsza) (1.0 /. b.najwieksza); rozlaczne = true}
   (* b jest zbiorem rozłącznym *)
   else 
      sumazbiorow (podzielic a {najmniejsza = neg_infinity; najwieksza = b.najmniejsza; rozlaczne =false}) 
                  (podzielic a {najmniejsza = b.najwieksza; najwieksza = infinity ; rozlaczne=false});;
   
(*KILKA TESTÓW :
let a = sr_wartosc ( podzielic ( wartosc_od_do (0.000000) (0.000000) ) ( podzielic ( wartosc_od_do (-7.600000) (-5.200000) ) ( plus ( podzielic ( wartosc_dokladnosc (-8.400000) (6.000000) ) ( wartosc_dokladna (0.000000) ) ) ( plus ( wartosc_dokladna (-2.000000) ) ( plus ( wartosc_od_do (-2.600000) (-1.400000) ) ( wartosc_od_do (-8.200000) (2.400000) ) ) ) ) ) ) ;;
assert ((classify_float a) == FP_nan);;
let a = sr_wartosc ( razy ( podzielic ( plus ( wartosc_dokladnosc (6.200000) (8.800000) ) ( wartosc_od_do (-7.400000) (-5.800000) ) ) ( wartosc_od_do (-6.000000) (0.000000) ) ) ( wartosc_dokladnosc (0.000000) (7.000000) ) ) ;;
assert (a = 0.);;
let a = max_wartosc ( podzielic ( wartosc_od_do (1.200000) (4.000000) ) ( minus ( wartosc_dokladna (-4.200000) ) ( wartosc_dokladna (-8.200000) ) ) ) ;;
assert (a = 1.00000000000000022);;
let a = max_wartosc ( plus ( wartosc_dokladna (-9.600000) ) ( wartosc_od_do (8.600000) (9.600000) ) ) ;;
assert (a = 0.);;
let a = in_wartosc ( plus ( wartosc_dokladna (-0.800000) ) ( plus ( podzielic ( wartosc_od_do (-8.200000) (3.200000) ) ( minus ( plus ( minus ( wartosc_dokladnosc (-0.800000) (1.200000) ) ( wartosc_dokladna (0.800000) ) ) ( wartosc_od_do (-0.600000) (1.400000) ) ) ( minus ( wartosc_dokladnosc (-0.600000) (0.800000) ) ( wartosc_od_do (-7.800000) (0.000000) ) ) ) ) ( wartosc_dokladna (-5.000000) ) ) ) (-7.800000);;
assert (a = true);;
let a = sr_wartosc ( minus ( wartosc_od_do (-7.600000) (0.200000) ) ( wartosc_dokladnosc (-5.600000) (3.000000) ) ) ;;
assert (a = 1.9);;
let a = min_wartosc ( podzielic ( plus ( podzielic ( razy ( wartosc_dokladna (-10.000000) ) ( podzielic ( wartosc_od_do (-4.600000) (0.400000) ) ( wartosc_dokladnosc (5.200000) (0.000000) ) ) ) ( wartosc_dokladna (-9.600000) ) ) ( podzielic ( wartosc_dokladna (6.000000) ) ( wartosc_od_do (0.000000) (6.200000) ) ) ) ( wartosc_dokladnosc (-5.000000) (0.000000) ) ) ;;
assert (a = neg_infinity);;
let a = in_wartosc ( minus ( wartosc_dokladnosc (1.400000) (1.200000) ) ( wartosc_od_do (3.800000) (8.000000) ) ) (-5.200000);;
assert (a = true);;   *)
