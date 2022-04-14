[<AutoOpen>]
module Primacy.Symfig.Schema

type EnvVars<'value> = Map<string, 'value>

module EnvVars =
  let choose f (envVars : EnvVars<_>) : EnvVars<_> =
    let folder s k v =
      let result = f (k,v)
      if Option.isSome result
      then Map.add k result.Value s
      else s
    envVars |> Map.fold folder Map.empty

type EnvErrors<'details> = {| Key: string; Details: 'details |} list

module EnvErrors =
  let print (envErrors : EnvErrors<_>) =
    envErrors
    |> List.map (sprintf "%A")
    |> String.concat System.Environment.NewLine

type KeyOptions = {
  Prefix : string option
  Append : string -> string -> string
}
