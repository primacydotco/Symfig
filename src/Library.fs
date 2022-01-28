module Library

open Microsoft.FSharp.Reflection

module Env =
  let key head tail = $"{head}__{tail}"

  let get key env =
    match env key with
    | Some v -> v
    | None -> invalidOp $"Options: No environment value was found with key '{key}'."

let private isOptional (t : System.Type) =
  t.IsGenericType &&
  t.GetGenericTypeDefinition() = typedefof<Option<_>>

let private getOptionalType (t : System.Type) =
  if t.GenericTypeArguments.Length <> 1 then failwith "Optional types only have one type argument"
  t.GenericTypeArguments.[0]

/// https://stackoverflow.com/a/51976314/1259408
let private mkOption optionType value isSome =
  let cases = FSharp.Reflection.FSharpType.GetUnionCases(optionType)
  let cases = cases |> Array.partition (fun x -> x.Name = "Some")
  let someCase = fst cases |> Array.exactlyOne
  let noneCase = snd cases |> Array.exactlyOne
  let relevantCase, args =
      match isSome with
      | true -> someCase, [| value |]
      | false -> noneCase, [| |]
  FSharp.Reflection.FSharpValue.MakeUnion(relevantCase, args)

let private traverseResults f array =
    let folder head tail = f head |> Result.bind (fun h -> tail |> Result.bind (fun t -> Array.append [| h |] t |> Ok))
    Array.foldBack folder array (Ok Array.empty)

let write key (o : 'a)  =
  let rec loop t pfx o =
    if FSharpType.IsRecord t then
      FSharpType.GetRecordFields t
      |> Array.toList
      |> List.collect (fun p ->
        FSharpValue.GetRecordField (o, p) |> loop p.PropertyType (Env.key pfx p.Name))
    elif FSharpType.IsTuple t then
      FSharpType.GetTupleElements t
      |> Array.toList
      |> List.mapi (fun i t -> t, FSharpValue.GetTupleField (o, i))
      |> List.collect (fun (t, f) -> loop t pfx f)
    elif t = typeof<bool> && typeof<'value> = typeof<string> then
      let value = if o :?> bool then "true" else "false"
      [ pfx, box value :?> 'value ]
    elif t.IsAssignableTo typeof<string seq> && typeof<'value> = typeof<string> then
      let value = o :?> string seq |> String.concat ";"
      [ pfx, box value :?> 'value ]
    else
      if isOptional t && o = null then []
      elif isOptional t then
        let innerType = getOptionalType t
        let value = t.GetProperty("Value").GetValue(o)
        loop innerType pfx value
      else [ pfx, o :?> 'value ]

  loop typeof<'a> key o


let read<'t> prefix (env : _) : 't =
  let rec loop t pfx : Result<obj, string> =
    if isOptional t then
      loop (getOptionalType t) pfx
      |> function
      | Ok v -> Ok <| mkOption t v true
      | Error _ -> Ok <| mkOption t null false
    elif FSharpType.IsRecord t then
      FSharpType.GetRecordFields t
      |> traverseResults (fun p -> loop p.PropertyType (Env.key pfx p.Name))
      |> Result.map (fun fs -> FSharpValue.MakeRecord (t, fs))
    elif FSharpType.IsTuple t then
      FSharpType.GetTupleElements t
      |> traverseResults (fun e -> loop e pfx)
      |> Result.map (fun fs -> FSharpValue.MakeTuple (fs, t))
    else
      let var = env pfx
      if t = typeof<bool> then
        match var with
        | Some "true" | Some "TRUE" | Some "1" ->
          Ok <| upcast true
        | _ ->
          Ok <| upcast false
      elif t.IsAssignableTo typeof<string seq> then
        let value =
          match var with
          | Some text -> text.Split ";"
          | None -> Array.empty
        if t = typeof<string list>
        then
          Ok <| upcast List.ofArray value
        else
          Ok <| upcast value
      else
        match var with
        | Some v -> Ok <| upcast v
        | None -> Error <| $"No environment value was found with key '{pfx}' and it is not optional."

  match loop typeof<'t> prefix with
  | Ok result -> result :?> 't
  | Error e -> invalidOp e
