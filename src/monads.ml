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
  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m;;
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

module MakeUtils(M : Monad) : Utils with type 'a m = 'a M.m =
struct
  type 'a m = 'a M.m;;
  let return = M.pure;;
  let (>>=) = M.bind;;
  let rec sequence xms =
    let open M in
    match Enum.get xms with
    | None -> return @@ Enum.empty ()
    | Some xm ->
      let%bind x = xm in
      let%bind xs = sequence xms in
      return @@ Enum.append (Enum.singleton x) xs
  ;;
  let mapM f xs = sequence (Enum.map f xs);;
  let lift1 f m = M.bind m @@ fun x -> M.pure @@ f x;;
end;;
