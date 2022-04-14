module Primacy.Symfig.Config

open Xunit

type SampleRecordX<'v> = {
  ConfigA : 'v
  ConfigB : {|
    Config1 : 'v
    Config2 : 'v
  |}
  ConfigC : 'v option
  ConfigD : 'v option
}

type ReadableSampleRecordX = SampleRecordX<string>
type WriteableSampleRecordX<'secret> = SampleRecordX<SampleEnvVar<'secret>>
and SampleEnvVar<'v> =
  | PlainText of string
  | Secret of 'v

let sampleRecordX : WriteableSampleRecordX<int> = {
  ConfigA = PlainText "cat"
  ConfigB = {|
    Config1 = PlainText "boop"
    Config2 = Secret 42
  |}
  ConfigC = Some <| PlainText "dog"
  ConfigD = None
}

let sampleVariablesX = Map [
  "X__CONFIGA", PlainText "cat"
  "X__CONFIGB__CONFIG1", PlainText "boop"
  "X__CONFIGB__CONFIG2", Secret 42
  "X__CONFIGC", PlainText "dog"
]

let sampleKeyOptionsX = {
  Prefix = Some "X"
  // You can format keys as you like
  // https://maxdeviant.com/posts/2019/implementing-a-case-conversion-library-in-fsharp-and-haskell/
  Append = fun a b -> $"{a}__{b.ToUpperInvariant()}"
}

let sampleEnvReader (written : Result<EnvVars<_>, EnvErrors<string>>) (key : string) =
  match written with
  | Ok vars -> vars |> Map.tryFind key
  | Error _ -> None

[<Fact>]
let ``writes`` () =
  let result = Config.write<SampleEnvVar<int>, WriteableSampleRecordX<int>> [ ] sampleKeyOptionsX sampleRecordX
  Assert.Equal (Ok sampleVariablesX, result)

[<Fact>]
let ``reads`` () =
  let result = Config.read<SampleEnvVar<int>, WriteableSampleRecordX<int>> [ ] sampleKeyOptionsX (fun k -> sampleVariablesX |> Map.tryFind k)
  Assert.Equal (Ok sampleRecordX, result)

[<Fact>]
let ``read non-optional returns error`` () =
  let target = Config.read<string, {| A : string; B: {| Y: string; Z: string |}; C: string option |}> [ ]
  let targetNonOptional = 3 // Three properties in our target which aren't optional.

  match target sampleKeyOptionsX (fun _ -> None) with
  | Ok _ -> Assert.True false
  | Error e -> Assert.Equal (targetNonOptional, e.Length)

[<Fact>]
let ``read optional`` () =
  let result = Config.read<string, {| A : string |} option> [ ] sampleKeyOptionsX (fun _ -> None)

  match result with
  | Ok r -> Assert.True r.IsNone
  | Error e -> Assert.True false


[<Fact>]
let ``write optional none omits`` () =
  let result =
    {| A = "a"; B = None |}
    |> Config.write<string, _> [ ] sampleKeyOptionsX
    |> Result.map Map.toList

  match result with
  | Ok r -> Assert.Equal<_ list> ([("X__A", "a")], r)
  | Error e -> Assert.True false

type SampleConfig = {
    ConfigA : string
    ConfigB : {|
      Config1 : string
      Config2 : bool
    |}
    ConfigC : string option
  }

[<Fact>]
let ``basic usage example works`` () =
  let config = {
    ConfigA = "Test"
    ConfigB = {|
      Config1 = "cat"
      Config2 = true
    |}
    ConfigC = None
  }

  let options = {
    Prefix = Some "MY"
    Append = fun a b -> $"{a}__{b}"
  }

  let values = Config.String.write options config
  // values = Ok (Map [
  //   "MY__CONFIGA", "TEST"
  //   "MY__CONFIGB__CONFIG1", "cat"
  //   "MY__CONFIGB__CONFIG2", true
  // ])

  let config' = Config.String.read<SampleConfig> options (sampleEnvReader values)

  Assert.Equal (Ok config, config')
