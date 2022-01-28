[<AutoOpen>]
module internal fx

type Validation<'a, 'err> = Result<'a, 'err list>

[<RequireQualifiedAccess>]
module Validation =

  let ok a : Validation<_, _> = Ok a
  let error e : Validation<_, _> = List.singleton e |> Error

  let apply f (a: Validation<_, _>) : Validation<_, _> =
    match f, a with
    | Ok f, Ok a -> Ok (f a)
    | Error e, Ok _
    | Ok _, Error e -> Error e
    | Error e1, Error e2 -> Error (e1 @ e2)

  // https://fsharpforfunandprofit.com/posts/elevated-world-4/#traverse
  // Thanks Scott.
  let rec traverseA (f : 'a -> Validation<'b, _>) (a : 'a list) : Validation<'b list, _> =
    let (<*>) = apply
    let retn = ok
    let cons head tail = head :: tail

    match a with
    | [] ->
      retn []
    | head::tail ->
      retn cons <*> (f head) <*> (traverseA f tail)
