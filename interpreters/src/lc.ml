open Camlrack
open Camlrack.ListConvenienceFunctions

(*

   SYNTAX

   e ::= x
      |  (λ (x) e)
      |  (e e)

   v ::= (λ (x) e)

*)

type exp =
  | Var of string
  | Lam of string * exp
  | App of exp * exp

let is_value (e : exp) : bool =
  match e with
  | Lam _ -> true
  | _ -> false

let rec parse (s : sexp) : exp =
  match%spat s with
  | "SYMBOL" -> Var (sexp_to_symbol s)
  | "(lambda (SYMBOL) ANY)" ->
    Lam (sexp_to_symbol (first (sexp_to_list (second (sexp_to_list s)))),
         parse (third (sexp_to_list s)))
  | "(ANY ANY)" ->
    App (parse (first (sexp_to_list s)),
         parse (second (sexp_to_list s)))
  | _ -> failwith "invalid input"

let rec alpha (within : exp) (renaming : string) (renamed : string) : exp =
  match within with
  | Var x ->
    if x = renaming
    then Var renamed
    else Var x
  | Lam (x, e) ->
    if x = renaming
    then Lam (x, e)
    else Lam (x, alpha e renaming renamed)
  | App (e1, e2) ->
    App (alpha e1 renaming renamed,
         alpha e2 renaming renamed)

let rec beta (what : exp) (replacing : string) (within : exp) : exp =
  match within with
  | Var x ->
    if x = replacing
    then what
    else Var x
  | Lam (x, e) ->
    if x = replacing
    then Lam (x, e)
    else Lam (x, beta what replacing e)
  | App (e1, e2) ->
    App (beta what replacing e1,
         beta what replacing e2)

let genvar = ref 0

let fresh () =
  genvar := !genvar +1;
  "_v" ^ string_of_int !genvar

(*

   SEMANTICS (SMALL-STEP)

        e1 -> e1'
   ------------------- E-App1
   (e1 e2) -> (e1' e2)

        e2 -> e2'
   ------------------- E-App1'
   (e1 e2) -> (e1' e2)

       e -> e'
   --------------- E-App2
   (v e) -> (v e')

   ---------------------------- E-Sub
   ((lambda (x) e) v) -> e[x/v]

*)

let rec smallstep (e : exp) : exp =
  match e with
  | Var _ ->
    failwith "free variable"
  | Lam _ ->
    e
  | App (Lam (x, e), v)
    when is_value v ->
    let v' = alpha v x (fresh ()) in
    beta v' x e
  | App (v, e)
    when is_value v ->
    App (v, smallstep e)
  | App (e1, e2) ->
    App (smallstep e1, e2)

let rec interp_smallstep (e : exp) : exp =
  if is_value e
  then e
  else interp_smallstep (smallstep e)

let eval_smallstep s = interp_smallstep (parse (sexp_of_string_exn s))

(*

   SEMANTICS (BIG-STEP)

   e1 => (lambda (x) e)    e2 => v1    e[x/v1] => v2
   -------------------------------------------------
                     (e1 e2) => v2

*)

let rec bigstep (e : exp) : exp =
  match e with
  | Var _ ->
    failwith "free variable"
  | Lam _ ->
    e
  | App (e1, e2) ->
    (match bigstep e1 with
     | Lam (x, e') ->
       let v1 = bigstep e2 in
       beta v1 x e'
     | _ -> failwith "invalid input")

let interp_bigstep (e : exp) : exp = bigstep e

let eval_bigstep s = interp_bigstep (parse (sexp_of_string_exn s))
