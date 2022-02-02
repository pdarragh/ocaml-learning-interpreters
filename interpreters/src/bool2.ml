(** This module defines and explains an interpreter for an extended Boolean
     algebra consisting of primitive Boolean values, conjunctions, and
     disjunctions. *)

open Camlrack
open Camlrack.ListConvenienceFunctions

(* Let's take the same Boolean algebra language from [bool.ml] and add one new
   functionality: the Boolean disjunction operator, [or]. One possible BNF for
   this language looks like this:

     v ::= T | F

     e ::= v
        |  (and e e)
        |  (or e e)

   Almost identical to what we had previously, except we've added the [or] form!
   However, we could imagine going on to later add other binary Boolean
   operators (such as [nand], [xor], etc.). Things could get a bit tedious that
   way, so let's instead define our language's syntax a bit differently:

     v ::= T | F

     op ::= and | or

     e ::= v
        |  (op e e)

   Now we've defined an [op] metavariable. Using this, we can express all
   possible binary Boolean operators with a single [e] form. This will come in
   handy later!

   (There are tricks like this that show up the more you work with operational
   semantics. They're not always easy to arrive at by yourself; some of them you
   may just see other people do and eventually come to appreciate. We'll try to
   guide you in this regard based on our experiences, so don't feel bad if this
   seems unintuitive or strange at first!)

   Now, when we implement our expression form, we will do something a little bit
   different than last time... *)

(** The type of expressions for our extended Boolean algebra. *)
type exp =
  (* These are the same primitive constructors we had previously. *)
  | T | F
  (* But now we've added a constructor for binary operators, which relies on a
     new [op] type. *)
  | BinOp of op * exp * exp
(** The type of operators. *)
and op =
  (* We provide one argument-less constructor for each operator. *)
  | And | Or
  (* You can imagine if we wanted to define additional binary Boolean operators,
     we would only need to extend this part of the type! *)

(* Next, we'll extend our parser. Even though we've changed things around a bit,
   the parsing step is pretty straightforward! *)

(** Parses a [string] to an [exp], if possible. Raises an exception on
    failure. *)
let parse (s : string) : exp =
  let rec parse' (se : sexp) : exp =
    match%spat se with
    | "T" -> T
    | "F" -> F
    | "(and ANY ANY)" ->
      BinOp (And,
             parse' (second (sexp_to_list se)),
             parse' (third (sexp_to_list se)))
    | "(or ANY ANY)" ->
      BinOp (Or,
             parse' (second (sexp_to_list se)),
             parse' (third (sexp_to_list se)))
    | _ -> failwith "parse: invalid input"
  in
  parse' (Camlrack.sexp_of_string_exn s)

(* We will also write a new [exp]-to-[string] function. *)

(** Converts an [exp] into a [string]. *)
let rec string_of_exp (a : exp) : string =
  let string_of_op (o : op) : string =
    match o with
    | And -> "and"
    | Or -> "or"
  in
  match a with
  | T -> "T"
  | F -> "F"
  | BinOp (o, l, r) -> "(" ^ string_of_op o ^
                       " " ^ string_of_exp l ^
                       " " ^ string_of_exp r ^ ")"

(* Now we come to the interesting part: defining an operational semantics for
   our language.

   In our previous iteration, we defined the Boolean conjunction operator [and]
   manually. This required five separate semantic rules to handle correctly. If
   we used the same strategy to implement our disjunction operator [or], it
   would require another five rules! I don't think we want to go through all
   that effort if there is an easier way.

   In cases like this, we can refer to a [metalanguage]. A metalanguage is
   another language that we assume to exist that can help us with certain
   things. For this example, what we want a metalanguage for is to interpret the
   results of Boolean conjunction and disjunction. This is a well-known relation
   for almost anybody in computer science, so it should not be problematic to
   assume that other computer scientists will understand what we mean!

   We will use a special bracket notation, ⟪ and ⟫, to indicate when a syntactic
   term from our language should be converted into our metalanguage, which for
   this exercise will be OCaml. Our operational semantics will now look like
   this:

           e1 -> e1'
   -------------------------  E-OpLeft
   (op e1 e2) -> (op e1' e2)

            e -> e'
     ---------------------     E-OpRight
     (op v e) -> (op v e')

   ⟪ v3 ⟫ = ⟪ v1 ⟫ && ⟪ v2 ⟫
   --------------------------  E-And
       (and v1 v2) -> v3

   ⟪ v3 ⟫ = ⟪ v1 ⟫ || ⟪ v2 ⟫
   --------------------------  E-Or
        (or v1 v2) -> v3

   In this versino of our operational semantics, we are relying on the existence
   of the [&&] and [||] operators to do our dirty work. We "inject" our
   syntactic terms into the metalangauge (OCaml) using the ⟪ ⟫ notation.

   To use this technique, we're going to need a function to convert from
   syntactic values to OCaml, then a function to convert back from OCaml to our
   syntactic values.*)

(** Converts an [exp] to an OCaml [bool]. *)
let ocaml_bool_of_exp (a : exp) : bool =
  match a with
  | T -> true
  | F -> false
  | _ -> failwith "ocaml_bool_of_exp: invalid term"

(** Converts an OCaml [bool] to an [exp]. *)
let exp_of_ocaml_bool (b : bool) : exp =
  match b with
  | true -> T
  | false -> F

(* Next, we will copy our [is_value] function from the previous implementation.
   This will come in handy when we get to the [step] function. *)

(** Determines whether an expression [a] is actually a value. *)
let is_value (a : exp) : bool =
  match a with
  | T | F -> true
  | _ -> false

(* And now, we can implement the [step] function. *)

(** Takes a single step according to the small-step operational semantics. *)
let rec step (a : exp) : exp =
  let get_op (o : op) : (bool -> bool -> bool) =
    match o with
    | And -> (&&)
    | Or -> (||)
  in
  match a with
  | BinOp (op, v1, v2)          (* E-And and E-Or *)
    when is_value v1 && is_value v2 ->
    exp_of_ocaml_bool ((get_op op) (ocaml_bool_of_exp v1) (ocaml_bool_of_exp v2))
  | BinOp (op, v, e)            (* E-OpRight *)
    when is_value v ->
    let e' = step e in
    BinOp (op, v, e')
  | BinOp (op, e1, e2) ->       (* E-OpLeft *)
    let e1' = step e1 in
    BinOp (op, e1', e2)
  | _ -> a

(* And, again, we will define a [multistep] function to help us take multiple
   steps in a single function call. This is identical to the previous one! *)

(** Takes [k] steps over the given expression [a].

    NOTE: If [k] is negative, this function will continue to recurse until the
    reduction is complete. Be careful of any adversarial inputs! *)
let rec multistep (k : int) (a : exp) : exp =
  if is_value a
  then a
  else
    match k with
    | 0 -> a
    | _ ->
      let a' = step a in
      multistep (k - 1) a'
