open Jhupllib_pp_utils;;

module type Uid_module =
sig
  type t
  val fresh : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t pretty_printer
  val show : t -> string
  val to_int : t -> int
end;;

module Make () : Uid_module =
struct
  type t = int [@@deriving eq, ord, show]
  let counter = ref 0;;
  let fresh () =
    let x = !counter + 1 in
    counter := x;
    x
  ;;
  let to_int x = x;;
end;;
