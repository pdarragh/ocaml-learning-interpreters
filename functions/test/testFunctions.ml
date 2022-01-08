open Camlrack
open Either
open OUnit2

open Functions

module type TestInterface = sig
  type input_type
  type result_type
  val name : string
  val test_fn : (input_type * (result_type, exn) Either.t) -> 'a -> unit
  val test_inputs : (input_type * (result_type, exn) Either.t) list
end

module Funcs = struct
  let double_def = parse_fundef [%sexp "{define {double x} {+ x x}}"]
  let quadruple_def = parse_fundef [%sexp "{define {quadruple x} {double {double x}}}"]
end

module TestGetFundef = struct
  type input_type = string * (func_defn list)
  type result_type = func_defn

  let name = "get_fundef"

  let test_fn ((target, defs), result) _ =
    match result with
    | Left fd -> assert_equal (get_fundef target defs) fd
    | Right exn -> assert_raises exn (fun () -> get_fundef target defs)

  let test_inputs =
    let double_def = Funcs.double_def in
    let quadruple_def = Funcs.quadruple_def in
    [ (("double", [double_def]), Left double_def)
    ; (("double", [double_def; quadruple_def]), Left double_def)
    ; (("double", [quadruple_def; double_def]), Left double_def)
    ; (("quadruple", [quadruple_def; double_def]), Left quadruple_def)
    ; (("double", []), Right (Failure "undefined function"))
    ]
end

module TestSubst = struct
  type input_type = sexp * symbol * sexp
  type result_type = sexp

  let name = "subst"

  let test_fn ((what, within, in_place_of), result) _ =
    match result with
    | Left sexp -> assert_equal (subst (parse what) within (parse in_place_of)) (parse sexp)
    | Right exn -> assert_raises exn (fun () -> subst (parse what) within (parse in_place_of))

  let test_inputs =
    [ (([%sexp "8"], "x", [%sexp "9"]), Left [%sexp "9"])
    ; (([%sexp "8"], "x", [%sexp "x"]), Left [%sexp "8"])
    ; (([%sexp "8"], "x", [%sexp "y"]), Left [%sexp "y"])
    ; (([%sexp "8"], "x", [%sexp "{+ x y}"]), Left [%sexp "{+ 8 y}"])
    ; (([%sexp "8"], "x", [%sexp "{* y x}"]), Left [%sexp "{* y 8}"])
    ; (([%sexp "8"], "x", [%sexp "{double x}"]), Left [%sexp "{double 8}"])
    ]
end

module TestParse = struct
  type input_type = sexp
  type result_type = exp

  let name = "parse"

  let test_fn (input, result) _ =
    match result with
    | Left exp -> assert_equal (parse input) exp
    | Right exn -> assert_raises exn (fun () -> parse input)

  let test_inputs =
    [ ([%sexp "2"], Left (Eint 2))
    ; ([%sexp "x"], Left (Eid "x"))
    ; ([%sexp "{+ 2 1}"], Left (Eplus (Eint 2, Eint 1)))
    ; ([%sexp "{* 3 4}"], Left (Emult (Eint 3, Eint 4)))
    ; ([%sexp "{+ {* 3 4} 8}"], Left (Eplus (Emult (Eint 3, Eint 4), Eint 8)))
    ; ([%sexp "{double 9}"], Left (Eapp ("double", Eint 9)))
    ; ([%sexp "{{+ 1 2}}"], Right (Failure "invalid input"))
    ]
end

module TestParseFundef = struct
  type input_type = sexp
  type result_type = func_defn

  let name = "parse_fundef"

  let test_fn (input, result) _ =
    match result with
    | Left fd -> assert_equal (parse_fundef input) fd
    | Right exn -> assert_raises exn (fun () -> parse_fundef input)

  let test_inputs =
    [ ([%sexp "{define {double x} {+ x x}}"],
       Left { name = "double"
            ; arg = "x"
            ; body = Eplus (Eid "x", Eid "x") })
    ; ([%sexp "{def {f x} x}"],
       Right (Failure "invalid input"))
    ]
end

module TestInterp = struct
  type input_type = sexp * (func_defn list)
  type result_type = int

  let name = "interp"

  let test_fn ((input, defs), result) _ =
    match result with
    | Left n -> assert_equal (interp (parse input) defs) n
    | Right exn -> assert_raises exn (fun () -> interp (parse input) defs)

  let test_inputs =
    let double_def = Funcs.double_def in
    let quadruple_def = Funcs.quadruple_def in
    [ (([%sexp "2"], []), Left 2)
    ; (([%sexp "x"], []), Right (Failure "free variable"))
    ; (([%sexp "{+ 2 1}"], []), Left 3)
    ; (([%sexp "{* 2 1}"], []), Left 2)
    ; (([%sexp "{+ {* 2 3} {+ 5 8}}"], []), Left 19)
    ; (([%sexp "{double 8}"], [double_def]), Left 16)
    ; (([%sexp "{quadruple 8}"], [double_def; quadruple_def]), Left 32)
    ]
end

let interfaces : (module TestInterface) list =
  [ (module TestGetFundef)
  ; (module TestSubst)
  ; (module TestParse)
  ; (module TestParseFundef)
  ; (module TestInterp)
  ]

let test_suite =
  let prep_tests (base_index, tests) ((module Interface) : (module TestInterface)) =
    let new_tests =
      List.mapi
        (fun i p -> ("test:" ^ string_of_int (base_index + i) ^
                     "::" ^ Interface.name ^ "_test:" ^ string_of_int (i + 1))
                    >:: (Interface.test_fn p))
        Interface.test_inputs in
    (base_index + List.length new_tests, List.append tests new_tests) in
  "interp1" >::: snd (List.fold_left prep_tests (1, []) interfaces)

let () = run_test_tt_main test_suite
