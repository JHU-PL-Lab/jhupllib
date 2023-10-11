(**
   This module contains an interface for monads.
*)

open Batteries;;

module type Monad = sig
  type 'a m;;

  val pure : 'a -> 'a m;;
  val bind : 'a m -> ('a -> 'b m) -> 'b m;;
end;;

module type MonadPlus = sig
  type 'a m;;

  include Monad with type 'a m := 'a m;;

  val zero : unit -> 'a m;;
  val plus : 'a m -> 'a m -> 'a m;;
end;;

module type Utils = sig
  type 'a m;;

  val return : 'a -> 'a m;;
  val sequence : 'a m Enum.t -> 'a Enum.t m
  val mapM : ('a -> 'b m) -> 'a Enum.t -> 'b Enum.t m
  val lift1 : ('a -> 'b) -> 'a m -> 'b m
end;;

module type MonadWithUtils = sig
  type 'a m;;
  include Monad with type 'a m := 'a m;;
  include Utils with type 'a m := 'a m;;
end;;

module type MonadPlusWithUtils = sig
  type 'a m;;
  include MonadPlus with type 'a m := 'a m;;
  include Utils with type 'a m := 'a m;;
end;;

module MakeUtils : functor (M : Monad) -> Utils with type 'a m = 'a M.m;;