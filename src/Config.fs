[<AutoOpen>]
module Primacy.Symfig.Config

open Microsoft.FSharp.Reflection

type EnvVars<'value> = Map<string, 'value>
type EnvErrors<'details> = {| Key: string; Details: 'details |} list
type KeyOptions = {
  Prefix : string option
  Append : string -> string -> string
}
type Encoder<'value> = obj -> Result<'value, string>
type TryEncoder<'value> = System.Type -> Encoder<'value> option
type Encoder<'t, 'value> = 't -> Result<'value, string>
type Decoder<'value> = 'value -> Result<obj, string>
type TryDecoder<'value> = System.Type -> Decoder<'value> option
type Decoder<'t, 'value> = 'value -> Result<'t, string>
type TryCodec<'value> = {
  Encode : TryEncoder<'value>
  Decode : TryDecoder<'value>
}
type Codec<'t, 'value> = {
  Encode : Encoder<'t, 'value>
  Decode : Decoder<'t, 'value>
}

let tryEncoder<'t, 'value> (encode : Encoder<'t, 'value>) : TryEncoder<'value> =
  fun t ->
    if t = typeof<'t>
    then Some <| fun o -> encode (o :?> 't)
    else None

let tryDecoder<'t, 'value> (decode : Decoder<'t, 'value>) : TryDecoder<'value> =
  fun t ->
    if t = typeof<'t>
    then Some <| fun o -> decode o |> Result.map (fun o -> upcast o)
    else None

let tryCodec<'t, 'value> (codec : Codec<'t, 'value>) : TryCodec<'value> = {
  Encode = tryEncoder codec.Encode
  Decode = tryDecoder codec.Decode
}

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

let private (|IsRecord|_|) t =
  if FSharpType.IsRecord t then Some t else None
let private (|IsTuple|_|) t =
  if FSharpType.IsTuple t then Some t else None
let private (|IsOptional|_|) t =
  if isOptional t then Some t else None
let private (|IsCodableWith|_|) (encoders : _ seq) t =
  encoders |> Seq.tryPick (fun enc -> enc t)
let private (|IsAssignableTo|_|) (b :System.Type) (a : System.Type) =
  if a.IsAssignableTo b then Some a else None

/// <summary>
/// Write a config as <see cref="EnvVar{'value}"/>.
/// </summary>
/// <typeparam name="'value">The type of values to return.</typeparam>
/// <typeparam name="'config">The type of config from which to fetch values.</typeparam>
/// <param name="encoders">Functions which, when given a type and a property of a <typeparamref name="'config"/>, may encode a <typeparamref name="'value"/>.</param>
/// <param name="opts">Options for defining keys.</param>
/// <param name="env">A function which, when given a key, returns a value.</param>
let write<'value, 'config> (encoders : TryEncoder<'value> seq) (opts : KeyOptions) (config : 'config) : Result<EnvVars<'value>, EnvErrors<string>> =
  let valueType = typeof<'value>
  let rec loop t key obj =
    match t, obj with
    | IsRecord t, obj ->
      FSharpType.GetRecordFields t
      |> Array.toList
      |> List.collect (fun p ->
        FSharpValue.GetRecordField (obj, p) |> loop p.PropertyType (opts.Append key p.Name))
    | IsTuple t, obj ->
      FSharpType.GetTupleElements t
      |> Array.toList
      |> List.mapi (fun i t -> t, FSharpValue.GetTupleField (obj, i))
      |> List.collect (fun (t, f) -> loop t key f)
    | IsOptional _, null ->
      []
    | IsOptional t, o ->
      let innerType = getOptionalType t
      let value = t.GetProperty("Value").GetValue(o)
      loop innerType key value
    | IsCodableWith encoders encode, o ->
      match encode o with
      | Ok v -> [ Ok (key, v) ]
      | Error e -> [ Error [ {| Key = key; Details = e |} ] ]
    | IsAssignableTo valueType _, o ->
      [ Ok (key, (o :?> 'value)) ]
    | t, _ ->
      [ Error [ {| Key = key; Details = $"No encoders available to encode a '{t.Name}' as a '{valueType.Name}'." |} ] ]

  loop typeof<'config> (defaultArg opts.Prefix "") config
  |> Validation.traverseA id
  |> Result.map Map

/// <summary>
/// Reads <typeparamref name="'value"/> from <paramref name="env"/> as a <typeparamref name="'config"/>.
/// </summary>
/// <typeparam name="'value">The type of values to return.</typeparam>
/// <typeparam name="'config">The type of config from which to fetch values.</typeparam>
/// <param name="decoders">Functions which, when given a type and a <typeparamref name="'value"/>, may decode a into properties of the <typeparamref name="'config"/>.</param>
/// <param name="opts">Options for defining keys.</param>
/// <param name="env">A function which, when given a key, returns a value.</param>
[<RequiresExplicitTypeArguments>]
let read<'value, 'config> (decoders : TryDecoder<'value> seq) (opts : KeyOptions) (env : string -> 'value option) : Result<'config, EnvErrors<string>> =
  let valueType = typeof<'value>
  let rec loop t key =
    match t, env key with
    | IsOptional t, _ ->
      loop (getOptionalType t) key
      |> function
      | Ok v -> Ok <| mkOption t v true
      | Error _ -> Ok <| mkOption t null false
    | IsRecord t, _ ->
      FSharpType.GetRecordFields t
      |> Array.toList
      |> Validation.traverseA (fun p -> loop p.PropertyType (opts.Append key p.Name))
      |> Result.map (fun fs -> FSharpValue.MakeRecord (t, List.toArray fs))
    | IsTuple t, _ ->
      FSharpType.GetTupleElements t
      |> Array.toList
      |> Validation.traverseA (fun e -> loop e key)
      |> Result.map (fun fs -> FSharpValue.MakeTuple (List.toArray fs, t))
    | IsCodableWith decoders decode, Some v ->
      match decode v with
      | Ok v -> Ok v
      | Error e -> Error [ {| Key = key; Details = e |} ]
    | IsAssignableTo valueType _, Some v ->
      Ok <| upcast v
    | t, Some _ ->
      Error [ {| Key = key; Details = $"No decoders available to decode a '{t.Name}' as a '{valueType.Name}'." |} ]
    | _, None ->
      Error [ {| Key = key; Details = $"No environment value with key '{key}' and it was not optional." |} ]

  loop typeof<'config> (defaultArg opts.Prefix "")
  |> Result.map (fun r -> r :?> 'config)

module String =

  module Codec =
    let ``string seq`` (seperator : string) = {
      Encode = fun (strings : string seq) ->
        match strings |> Seq.tryFind (fun str ->  str.Contains seperator) with
        | Some _ -> Error $"Value must not contain seperator '{seperator}' but it do."
        | None -> Ok <| String.concat ";" strings
      Decode = Ok << fun str -> str.Split seperator
    }

    let ``string list`` (seperator : string) =
      let c = ``string seq`` seperator
      {
        Encode = List.toSeq >> c.Encode
        Decode = c.Decode >> Result.map Seq.toList
      }

    let ``bool`` () = {
      Encode = Ok << function
        | true -> "true"
        | false -> "false"
      Decode = function
        | "true" | "TRUE" | "1" -> Ok true
        | "false" | "FALSE" | "0" -> Ok false
        | _ -> Error $"Value is not a valid boolean. Expected 'true', '1', 'false', or '0'."
    }

  let codecs = [
    tryCodec <| Codec.``string seq`` ";"
    tryCodec <| Codec.``string list`` ";"
    tryCodec <| Codec.``bool`` ()
  ]

  let encoders =
    codecs |> Seq.map (fun c -> c.Encode)

  let decoders =
    codecs |> Seq.map (fun c -> c.Decode)

  let write opts config =
    write encoders opts config

  [<RequiresExplicitTypeArguments>]
  let read<'config> opts env =
    read<string, 'config> decoders opts env
