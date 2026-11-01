(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = Euro of float

type dollar = Dollar of float

let dollar_to_euro (Dollar n) = Euro (n *. 0.89)

let euro_to_dollar (Euro n) = Dollar (n *. 1.12)

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
  | Pound of float
  | Swedish_crown of float

let to_pound x = match x with
  | Pound x -> Pound x
  | Yen x -> Pound (x *. 0.0048)
  | Swedish_crown x -> Pound (x *. 0.075)

(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
  | Nil
  | Int of int * intbool_list
  | Bool of bool * intbool_list

let test = Int(5, Bool(true, Bool(false, Int(7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = match ib_list with
  | Nil -> Nil
  | Int (x, xs) -> Int (f_int x, intbool_map f_int f_bool xs)
  | Bool (x, xs) -> Bool (f_bool x, intbool_map f_int f_bool xs)

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse sez = 
  let rec reverse_aux acc sez =
    match sez with
      | Nil -> acc
      | Int (x, xs) -> reverse_aux (Int (x, acc)) xs
      | Bool (x, xs) -> reverse_aux (Bool (x, acc)) xs
  in reverse_aux Nil sez

(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let intbool_separate ib_list = 
  let rec aux acc_i acc_b sez =
    match sez with
      | Nil -> acc_i, acc_b
      | Int (x, xs) -> aux (acc_i @ [x]) acc_b xs
      | Bool (x, xs) -> aux acc_i (acc_b @ [x]) xs
    in aux [] [] ib_list

(* Napiši še brez dvojne funkcije in akumulatorjev *)

(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = 
  | Fire
  | Frost
  | Arcane

type specialisation =
  | Historian
  | Teacher
  | Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status = 
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = {ime: string; status: status}

let professor = {ime = "Janez"; status = Employed (Fire, Historian)}
let jaina  = {ime = "Jaina"; status = Student (Fire, 4)}

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = { fire: int; frost: int; arcane: int}

let update counter magic_type = match magic_type with
  | Fire -> { counter with fire = counter.fire + 1}
  | Frost -> { counter with frost = counter.frost + 1}
  | Arcane -> { counter with arcane = counter.arcane + 1}

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic sez = 
  let rec aux counter sez = match sez with
    | [] -> counter
    | { status = Employed (magic, _) } :: carovniki -> aux (update counter magic) carovniki
    | { status = Student (magic, _) } :: carovniki -> aux (update counter magic) carovniki
    | x::xs -> aux counter xs
  in aux {fire = 0; frost = 0; arcane = 0} sez

(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let rec find_candidate magija polozaj seznam = 
  let leta = if polozaj = Historian then 3 else (if polozaj = Teacher then 5 else 4) in
  match seznam with
  | [] -> None
  | {ime = x; status = Student (magic, st)}::xs -> if magic = magija && st >= leta then Some x else find_candidate magija polozaj xs
  | x::xs -> find_candidate magija polozaj xs


(* let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
