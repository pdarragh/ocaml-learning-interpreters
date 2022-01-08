open Camlrack
open Camlrack.ListConvenienceFunctions

type symbol = string

type exp =
  | Eint of int
  | Eid of symbol
  | Eplus of exp * exp
  | Emult of exp * exp
  | Eapp of symbol * exp

type func_defn =
  { name : symbol
  ; arg  : symbol
  ; body : exp }

let rec get_fundef (n : symbol) (defs : func_defn list) : func_defn =
  match defs with
  | [] -> failwith "undefined function"
  | fd::defs' ->
    if fd.name = n
    then fd
    else get_fundef n defs'

let rec subst (what : exp) (in_place_of : symbol) (within : exp) : exp =
  match within with
  | Eint _ -> within
  | Eid s ->
    if s = in_place_of
    then what
    else within
  | Eplus (l, r) ->
    Eplus (subst what in_place_of l,
           subst what in_place_of r)
  | Emult (l, r) ->
    Emult (subst what in_place_of l,
           subst what in_place_of r)
  | Eapp (s, arg) ->
    Eapp (s, subst what in_place_of arg)

let rec parse (s : sexp) : exp =
  match%spat s with
  | "INTEGER" -> Eint (sexp_to_int s)
  | "SYMBOL" -> Eid (sexp_to_symbol s)
  | "{+ ANY ANY}" ->
    Eplus (parse (second (sexp_to_list s)),
           parse (third (sexp_to_list s)))
  | "{* ANY ANY}" ->
    Emult (parse (second (sexp_to_list s)),
           parse (third (sexp_to_list s)))
  | "{SYMBOL ANY}" ->
    Eapp (sexp_to_symbol (first (sexp_to_list s)),
          parse (second (sexp_to_list s)))
  | _ -> failwith "invalid input"

let parse_fundef (s : sexp) : func_defn =
  match%spat s with
  | "{define {SYMBOL SYMBOL} ANY}" ->
    { name = sexp_to_symbol (first (sexp_to_list (second (sexp_to_list s))))
    ; arg  = sexp_to_symbol (second (sexp_to_list (second (sexp_to_list s))))
    ; body = parse (third (sexp_to_list s)) }
  | _ -> failwith "invalid input"

let rec interp (a : exp) (defs : func_defn list) : int =
  match a with
  | Eint n -> n
  | Eid _ -> failwith "free variable"
  | Eplus (l, r) -> (interp l defs) + (interp r defs)
  | Emult (l, r) -> (interp l defs) * (interp r defs)
  | Eapp (s, arg) ->
    let fd = get_fundef s defs in
    interp (subst (Eint (interp arg defs)) fd.arg fd.body) defs

let interp_string (s : string) : int = interp (parse (sexp_of_string_exn s)) []
