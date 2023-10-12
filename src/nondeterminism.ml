(**
   This module contains a non-determinism monad.
*)

module Enum = Batteries.Enum;;

module LazyList = struct
  type 'a t = 'a node Lazy.t
  and 'a node = Nil | Cons of 'a * 'a t;;

  let empty : 'a t = Lazy.from_val Nil;;
  let singleton (x : 'a) : 'a t = Lazy.from_val (Cons(x,empty));;
  let rec map (f : 'a -> 'b) (xs : 'a t) : 'b t =
    let mapper (node : 'a node) : 'b node =
      match node with
      | Nil -> Nil
      | Cons(x, xs') -> Cons(f x, map f xs')
    in
    lazy (mapper (Lazy.force xs))
  let append (xs : 'a t) (ys : 'a t) : 'a t =
    let rec append' (xs : 'a t) (ys : 'a t) : 'a node =
      match Lazy.force xs with
      | Nil -> Lazy.force ys
      | Cons(x,xs') -> Cons(x, lazy(append' xs' ys))
    in
    lazy (append' xs ys)
  ;;
  let rec concat (xss : 'a t t) : 'a t =
    let concat' (xss : 'a t t) : 'a node =
      match Lazy.force xss with
      | Nil -> Nil
      | Cons(xs,xss') -> Lazy.force (append xs (concat xss'))
    in
    lazy (concat' xss)
  ;;
  let concat_map (f : 'a -> 'b t) (xs : 'a t) : 'b t = concat (map f xs);;
  let of_enum (e : 'a Enum.t) : 'a t =
    let rec of_enum' () : 'a node =
      match Enum.get e with
      | None -> Nil
      | Some x -> Cons(x, lazy (of_enum' ()))
    in
    Lazy.from_fun of_enum'
  ;;
  let enum (xs : 'a t) : 'a Enum.t =
    let unfolder (xs : 'a t) : ('a * 'a t) option =
      match Lazy.force xs with
      | Nil -> None
      | Cons(x,xs') -> Some(x, xs')
    in
    Enum.unfold xs unfolder
  ;;
  let of_list (xs : 'a list) : 'a t =
    xs
    |> BatList.enum
    |> of_enum
  ;;
end;;

module type Nondeterminism_monad_sig = sig
  type 'a m
  include Monads.MonadPlus with type 'a m := 'a m
  include Monads.Utils with type 'a m := 'a m
  val pick_enum : 'a Enum.t -> 'a m
  val enum : 'a m -> 'a Enum.t

  val stop_unless : bool -> unit m
  val empty : unit -> 'a m
  val alternative : 'a m -> 'a m -> 'a m
end;;

module Nondeterminism_monad : Nondeterminism_monad_sig = struct
  module Base = struct
    type 'a m = 'a LazyList.t;;
    let pure x = LazyList.singleton x;;
    let bind x f = LazyList.concat_map f x;;
    let zero () = LazyList.empty;;
    let plus = LazyList.append;;
  end;;
  include Base;;
  include Monads.MakeUtils(Base);;

  let pick_enum = LazyList.of_enum;;
  let enum = LazyList.enum;;

  let stop_unless x = if x then return () else zero ();;
  let empty () = LazyList.empty;;
  let alternative x y =
    LazyList.concat @@ LazyList.of_list [x;y]
  ;;
end;;
