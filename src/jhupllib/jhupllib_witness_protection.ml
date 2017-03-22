(**
   This module provides a means by which a registry of "witnesses" can be
   created.  Here, a witness is a representative of another (typically more
   complex) value.  A witness registry is a mutable structure which
   monotonically accumulates values, mapping distinct values to distinct
   witnesses.  The primary use of such a registry is to accelerate comparison
   operations.  For instance, using witnesses as keys in a tree-based dictionary
   may be faster than using the original values if the comparison between two
   values is an expensive operation.
*)

open Batteries;;

(**
   This module type describes the information required to create a witness
   registry module.
*)
module type Spec =
sig
  type t
  val compare : t -> t -> int
end;;

(**
   This is the type of a witness registry module.
*)
module type Registry =
sig
  (** The type of a witness registry. *)
  type t

  (** The type of elements stored in the registry. *)
  type elt

  (** The type of a witness in the registry. *)
  type witness

  (** A function to produce an empty witness registry.  Registries use mutable
      data structures to cache results, so each empty registry must be created
      separately. *)
  val empty_registry : unit -> t

  (** Obtains a witness for the provided value.  If the value already has a
      witness, it is returned; otherwise, a new witness is created and
      returned.  If the same element is added to two different registries, it
      will not be given the same witness for each. *)
  val witness_of : t -> elt -> witness

  (** Obtains a value for the provided witness.  Raises Not_found if no such
      witness is stored in the provided registry. *)
  val element_of : t -> witness -> elt

  (** Determines if two witnesses are equal.  Two witnesses are equal only if
      they witness the same value. *)
  val equal_witness : witness -> witness -> bool

  (** Compares two witnesses.  This comparison operation is arbitrary; although
      the element type must be comparable, there is no guarantee of a connection
      between the comparison of elements and the comparison of their
      witnesses. *)
  val compare_witness : witness -> witness -> int
end;;

(**
   A functor which creates witness registries.
*)
module Make(S : Spec) : Registry with type elt = S.t =
struct
  type elt = S.t;;
  type witness = int;;

  let equal_witness : int -> int -> bool =
    (* This use of "==" (instead of "=") is intentional: OCaml's == is a single
       machine instruction and, on primitive integers, identity is equality. *)
    (==)
  ;;

  let compare_witness : int -> int -> int =
    Pervasives.compare
  ;;

  module Witness_ord =
  struct
    type t = witness
    let compare = compare_witness
  end;;

  module Witness_map = Map.Make(Witness_ord);;

  module Element_map = Map.Make(S);;

  let next_available_witness : witness ref = ref 0;;

  type t =
    { witness_to_value : elt Witness_map.t ref;
      value_to_witness : witness Element_map.t ref
    }
  ;;

  let empty_registry () =
    { witness_to_value = ref Witness_map.empty;
      value_to_witness = ref Element_map.empty;
    }
  ;;

  let witness_of (r : t) (x : elt) : witness =
    match Element_map.Exceptionless.find x !(r.value_to_witness) with
    | None ->
      let w = !next_available_witness in
      next_available_witness := w + 1;
      r.value_to_witness := Element_map.add x w !(r.value_to_witness);
      r.witness_to_value := Witness_map.add w x !(r.witness_to_value);
      w
    | Some w -> w
  ;;

  let element_of (r : t) (w : witness) : elt =
    Witness_map.find w !(r.witness_to_value)
  ;;
end;;
