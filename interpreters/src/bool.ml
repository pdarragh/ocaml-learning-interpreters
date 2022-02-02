(** This module defines (and explains) an interpreter for a simple Boolean
    algebra consisting of the primitive Boolean values and Boolean
    conjunctions. **)

open Camlrack
open Camlrack.ListConvenienceFunctions

(* First, we have to define a type for expressions in our language. These make
   up the AST, or Abstract Syntax Tree, of our interpreter's implementation. We
   use this representation for actually evaluating inputs. *)

(** The type of expressions for our small Boolean algebra. *)
type exp =
  (* Primitive Boolean values representing [true] and [false]. *)
  | T | F
  (* The Boolean conjunction operator, [and]. *)
  | And of exp * exp

(* Next, we'll write a parser. The parser is a function that takes some kind of
   input from the user and converts it into an AST (our [exp] type from above)
   so we can actually do things with it.

   You will almost never need to write a complete parser from scratch --- there
   are tons of libraries and tools available to make your life easier. For this
   class, we'll be using Camlrack to convert user-input strings into
   S-expressions as an intermediate representation, and then we will convert
   those S-expressions to instances of the [exp] AST type.

   (If you're familiar with parsing at all, you know that usually there is a
   "tokenization" step. That's what we're using the S-expressions for!)

   Because we're using an S-expression tokenizer, we'll have to change the
   syntax of our language just a bit. Atomic elements (e.g., values, such as [T]
   and [F]) will be written on their own, but compound elements (e.g.,
   [And]-expressions) will use a bracketed form with the operator in the first
   position. This is known as "Polish notation" or "prefix notation". For
   example, an [And] of a [T] and a [F] would be written as:

     (and T F)

   We can write this out in a full BNF grammar, since that's how we've learned
   to formalize the syntax of languages:

     v ::= T | F

     e ::= v
        |  (and e e)

   Here, we've defined the metavariable [v] to stand for "values" --- syntactic
   elements of our language that our semantics (which will come later) can
   perceive as being irreducible. The metavariable [e] stands for "expressions",
   and in our case anything that isn't a value is an expression.

   It is common for the values to also be included in the expressions, as we've
   done here, but never the other way around. This is because an interpreter is
   a function that takes expressions [e] as inputs and produces values [v] as
   outputs. *)

(** Parses a string to an [exp], if possible. Raises an exception on failure. *)
let parse (s : string) : exp =
  let rec parse' (se : sexp) : exp =
    match%spat se with
    | "T" -> T
    | "F" -> F
    | "(and ANY ANY)" ->
      And (parse' (second (sexp_to_list se)),
           parse' (third (sexp_to_list se)))
    | _ -> failwith "parse: invalid input"
  in
  (* Here, we use Camlrack to convert the string to an S-expression ([sexp]).
     Note that this function will raise an exception if the conversion fails,
     which is noted with the [_exn] ending to the function's name. *)
  parse' (Camlrack.sexp_of_string_exn s)

(* Sometimes it might be nice to reconstruct our syntactic language from the
   AST values, so we'll quickly define a function for that. This is not
   necessary, especially for our simple Boolean algebra language, but sometimes
   it makes debugging significantly easier! *)

(** Converts an [exp] into a [string]. *)
let rec string_of_exp (a : exp) : string =
  match a with
  | T -> "T"
  | F -> "F"
  | And (l, r) -> "(and " ^ string_of_exp l ^ " " ^ string_of_exp r ^ ")"

(* Now that we've parsed our input into a value in OCaml, we can write a
   function to interpret that input until we get a value.

   On paper, our interpreter's operation is defined by the language's semantics.
   We want to write a small-step operational semantics for our Boolean algebra.
   Typically, a small-step semantics makes a syntactic distinction between
   expressions [e] and values [v], so let's provide a
   Let's write out a small-step operational semantics for our Boolean algebra:

              e1 -> e1'
     ---------------------------  E-AndLeft
     (and e1 e2) -> (and e1' e2)

           --------------         E-AndFalse
           (and F e) -> F

               e -> e'
       -----------------------    E-AndTrueStep
       (and T e) -> (and T e')

           --------------         E-AndTrueTrue
           (and T T) -> T

           --------------         E-AndTrueFalse
           (and T F) -> F

   Notice that we do not define any rules for reducing values. It is generally
   assumed that any syntactic form that is a value cannot be reduced, so we do
   not write rules for them in a small-step semantics.

   Now look at the [E-AndLeft] rule. This rule is meant to be used when the two
   arguments to the [and] operator have not yet been reduced. It forces the
   first argument, [e1], to take a single step forward, then constructs a new
   [and] operation with that stepped-forward result. This rule will be used
   multiple times until the [e1] expression is a value, at which point we can
   use one of the other rules.

   This may be a bit confusing at first: after all, values are part of the
   definition of expressions, so couldn't an expression like [(and F T)] be
   reduced using [E-AndLeft] instead of [E-AndFalse]? The trick here is that we
   have not provided any rules for reducing values themselves, which means that
   there is no rule we can plug in for the [e1 -> e1'] step if [e1] is a value
   (i.e., [T] or [F]). [E-AndLeft] can only be used when the first argument to
   [and] is an un-reduced expression.

   By repeatedly using the above rules, we can evaluate any syntactically valid
   expression in our Boolean algebra. Let's implement a [step] function to turn
   these rules into code! *)

(** Takes a single step according to the small-step operational semantics. *)
let rec step (a : exp) : exp =
  (* Although we wrote the operational semantics rules in order from most
     general to least, we'll have to reverse their order to satisfy OCaml's
     rules for pattern-match conditions! These are evaluated in-order and stop
     when the first pattern is matched. *)
  match a with
  | And (T, F) -> F             (* E-AndTrueFalse *)
  | And (T, T) -> T             (* E-AndTrueTrue *)
  | And (T, e) ->               (* E-AndTrueStep *)
    let e' = step e in
    And (T, e')
  | And (F, _) -> F             (* E-AndFalse *)
  | And (e1, e2) ->             (* E-AndLeft *)
    let e1' = step e1 in
    And (e1', e2)
  | _ -> a                   (* In any other case, we just don't take a step. *)

(* We've implemented our small-step reduction in OCaml, but it only takes a
   single step at a time!

   Now we have to implement a second function to take multiple steps, which we
   will cleverly name [multistep]. This [multistep] function will take two
   arguments: the [exp] to be evaluated, and a number of steps to take before
   quitting (assuming the expression is not fully reduced to a value before
   taking that many steps). When we write the single-step reduction, we use the
   small arrow [->], but when we write a multi-step relation we will instead use
   the small arrow with a star: [->*]. This says "use the small-step relation
   multiple times."

   Before we implement [mulitstep], we need a way to know if a given expression
   [e] is actually a value [v]. Otherwise, our [step] function will always
   execute! So we'll implement a simple [is_value] function to check, and use
   that in the [multistep] function. *)

(** Determines whether an expression [a] is actually a value. *)
let is_value (a : exp) : bool =
  match a with
  | T | F -> true
  | _ -> false

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

(* Let's check our implementation with a test. First, we'll use operational
   semantics to determine the value of an input expression. Then, we'll run that
   expression through [multistep] and see if we get the same result!

   Here's our test input:

     (and (and T (and F T)) (and F T))

   First up: the manual method!

               ---------------  E-AndFalse
               (and F T) ->* F
           -----------------------  E-AndTrueFalse
           (and T (and F T)) ->* F
     ---------------------------------------  E-AndFalse
     (and (and T (and F T)) (and F T)) ->* F

   So we ought to be able to reduce this to [F]. Let's see what we get from
   utop!

     utop # multistep (-1) (parse "(and (and T (and F T)) (and F T))");;
     - : exp = F

   We got the correct result. Hooray! *)
