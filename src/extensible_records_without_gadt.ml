type _ key = ..

module type KEY = sig
  type t

  val proj : 'a key -> t option
  val compare : t -> t -> int
end

let registered_keys = ref []
let register_key key = registered_keys := key :: !registered_keys

type ('a, 'b) acc =
  | Init : 'a key * 'b key -> ('a, 'b) acc
  | Compare_left_with : 'a key * int -> ('a, 'b) acc
  | Compare_right_with : int * 'b key -> ('a, 'b) acc
  | Res : int -> ('a, 'b) acc

let compare : type a b. a key -> b key -> int =
 fun left right ->
  List.to_seq !registered_keys
  |> Seq.fold_lefti
       (fun (acc : (a, b) acc) i (module K : KEY) ->
         match acc with
         | Init (left, right) -> (
             match (K.proj left, K.proj right) with
             | Some left, Some right -> Res (K.compare left right)
             | Some _, None -> Compare_right_with (i, right)
             | None, Some _ -> Compare_left_with (left, i)
             | None, None -> acc)
         | Compare_right_with (j, right) -> (
             match K.proj right with
             | Some _ -> Res (Int.compare j i)
             | None -> acc)
         | Compare_left_with (left, j) -> (
             match K.proj left with
             | Some _ -> Res (Int.compare i j)
             | None -> acc)
         | _ -> acc)
       (Init (left, right))
  |> function
  | Res x -> x
  | _ ->
      raise
        (Invalid_argument
           "comparision with at least one unregistered key variant")
