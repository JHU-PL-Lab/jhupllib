open Batteries;;

module type Nondeterminism_monad_sig =
sig
  type 'a m
  include Monads.MonadPlus with type 'a m := 'a m
  include Monads.Utils with type 'a m := 'a m
  val pick_enum : 'a Enum.t -> 'a m
  val enum : 'a m -> 'a Enum.t

  val stop_unless : bool -> unit m
  val empty : unit -> 'a m
end;;

module Nondeterminism_monad_base
  : Monads.MonadPlus with type 'a m = 'a list =
struct
  type 'a m = 'a list;;
  let pure x = [x];;
  let bind x f = List.concat @@ List.map f x;;
  let zero () = [];;
  let plus x y = List.append x y;;
end;;

module Nondeterminism_monad : Nondeterminism_monad_sig =
struct
  include Nondeterminism_monad_base;;
  include (Monads.MakeUtils(Nondeterminism_monad_base) :
             Monads.Utils with type 'a m := 'a m);;
  let pick_enum = List.of_enum;;
  let enum = List.enum;;
  let stop_unless x = if x then [()] else [];;
  let empty () = [];;
end;;
