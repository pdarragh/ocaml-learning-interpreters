open Camlrack
open Camlrack.ListConvenienceFunctions

type symbol = string

type exp =
  | Eint of int
  | Eid of symbol
  | Eplus of exp * exp
  | Emult of exp * exp
  | Eapp of symbol * exp
  | Elet of symbol * exp * exp

type func_defn =
  { name : symbol
  ; arg  : symbol
  ; body : exp }

type binding =
  { name  : symbol
  ; value : int }

let bind (n : symbol) (v : int) : binding = { name = n; value = v }

type env = binding list

let mt_env = []

let extend_env = List.cons

let rec get_fundef (n : symbol) (defs : func_defn list) : func_defn =
  match defs with
  | [] -> failwith "undefined function"
  | fd::defs' ->
    if fd.name = n
    then fd
    else get_fundef n defs'

let rec lookup (n : symbol) (env : env) : int =
  match env with
  | [] -> failwith "free variable"
  | b :: env' ->
    if b.name = n
    then b.value
    else lookup n env'

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
  | "{let {[SYMBOL ANY]} ANY}" ->
    let bs = sexp_to_list (first (sexp_to_list (second (sexp_to_list s)))) in
    Elet (sexp_to_symbol (first bs),
          parse (second bs),
          parse (third (sexp_to_list s)))
  | _ -> failwith "invalid input"

let parse_fundef (s : sexp) : func_defn =
  match%spat s with
  | "{define {SYMBOL SYMBOL} ANY}" ->
    { name = sexp_to_symbol (first (sexp_to_list (second (sexp_to_list s))))
    ; arg  = sexp_to_symbol (second (sexp_to_list (second (sexp_to_list s))))
    ; body = parse (third (sexp_to_list s)) }
  | _ -> failwith "invalid input"

let rec interp (a : exp) (env : env) (defs : func_defn list) : int =
  match a with
  | Eint n -> n
  | Eid s -> lookup s env
  | Eplus (l, r) -> (interp l env defs) + (interp r env defs)
  | Emult (l, r) -> (interp l env defs) * (interp r env defs)
  | Eapp (s, arg) ->
    let fd = get_fundef s defs in
    interp fd.body (extend_env (bind fd.arg (interp arg env defs)) mt_env) defs
  | Elet (n, rhs, body) ->
    interp body (extend_env (bind n (interp rhs env defs)) env) defs
