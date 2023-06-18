type _ key = ..
type (_, _) eq = Eq : ('a, 'a) eq

module IMap = Map.Make (Int)

module type KEY = sig
  type t
  type r

  val proj : 'a key -> (t * ('a, r) eq) option
  val compare : t -> t -> int
end

let registered_keys = ref []
let register_key key = registered_keys := key :: !registered_keys

type ('a, 'b) acc =
  | Init : 'a key * 'b key -> ('a, 'b) acc
  | Compare_left_with : 'a key * int -> ('a, 'b) acc
  | Compare_right_with : int * 'b key -> ('a, 'b) acc
  | Res : ('a, 'b) Dmap.cmp -> ('a, 'b) acc

let compare : type a b. a key -> b key -> (a, b) Dmap.cmp =
 fun left right ->
  List.to_seq !registered_keys
  |> Seq.fold_lefti
       (fun (acc : (a, b) acc) i (module K : KEY) ->
         match acc with
         | Init (left, right) -> (
             match (K.proj left, K.proj right) with
             | Some (left, Eq), Some (right, Eq) ->
                 let x = K.compare left right in
                 Res (if x = 0 then Eq else if x < 0 then Lt else Gt)
             | Some _, None -> Compare_right_with (i, right)
             | None, Some _ -> Compare_left_with (left, i)
             | None, None -> acc)
         | Compare_right_with (j, right) -> (
             match K.proj right with
             | Some _ ->
                 let x = Int.compare j i in
                 Res (if x < 0 then Lt else Gt)
             | None -> acc)
         | Compare_left_with (left, j) -> (
             match K.proj left with
             | Some _ ->
                 let x = Int.compare j i in
                 Res (if x < 0 then Lt else Gt)
             | None -> acc)
         | _ -> acc)
       (Init (left, right))
  |> function
  | Res x -> x
  | _ ->
      raise
        (Invalid_argument
           "comparision with at least one unregistered key variant")

module Extensible_record = Dmap.Make (struct
  type 'a t = 'a key

  let compare = compare
end)

type _ key += Foo : int key

let () =
  register_key
    (module struct
      type t = unit
      type r = int

      let proj : type a. a key -> (t * (a, r) eq) option = function
        | Foo -> Some ((), Eq)
        | _ -> None

      let compare () () = 0
    end)

let () =
  let record = Extensible_record.empty in
  let record = Extensible_record.add Foo 3 record in
  assert (Extensible_record.find Foo record = 3)
