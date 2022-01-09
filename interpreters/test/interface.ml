module type TestInterface = sig
  type input_type
  type result_type
  val name : string
  val test_fn : (input_type * (result_type, exn) Either.t) -> 'a -> unit
  val test_inputs : (input_type * (result_type, exn) Either.t) list
end
