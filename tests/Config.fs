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

let sampleKeyOptionsX : KeyOptions = {
  Prefix = Some "X"
  // You can format keys as you like
  // https://maxdeviant.com/posts/2019/implementing-a-case-conversion-library-in-fsharp-and-haskell/
  Append = fun a b -> $"{a}__{b.ToUpperInvariant()}"
}

let sampleEnvReader (written : Result<EnvVars<SampleEnvVar<int>>, EnvErrors<string>>) (key : string) =
  match written with
  | Ok vars -> vars |> Map.tryFind key
  | Error _ -> None

[<Fact>]
let ``writes`` () =
  let result = write<SampleEnvVar<int>, WriteableSampleRecordX<int>> [ ] sampleKeyOptionsX sampleRecordX
  Assert.Equal (Ok sampleVariablesX, result)

[<Fact>]
let ``reads`` () =
  let result = read<SampleEnvVar<int>, WriteableSampleRecordX<int>> [ ] sampleKeyOptionsX (fun k -> sampleVariablesX |> Map.tryFind k)
  Assert.Equal (Ok sampleRecordX, result)

[<Fact>]
let ``read non-optional returns error`` () =
  let target = read<string, {| A : string; B: {| Y: string; Z: string |}; C: string option |}> [ ]
  let targetNonOptional = 3 // Three properties in our target which aren't optional.

  match target sampleKeyOptionsX (fun _ -> None) with
  | Ok _ -> Assert.True false
  | Error e -> Assert.Equal (targetNonOptional, e.Length)

[<Fact>]
let ``read optional`` () =
  let result = read<string, {| A : string |} option> [ ] sampleKeyOptionsX (fun _ -> None)

  match result with
  | Ok r -> Assert.True r.IsNone
  | Error e -> Assert.True false


[<Fact>]
let ``write optional none omits`` () =
  let result =
    {| A = "a"; B = None |}
    |> write<string, _> [ ] sampleKeyOptionsX
    |> Result.map Map.toList

  match result with
  | Ok r -> Assert.Equal<_ list> ([("X__A", "a")], r)
  | Error e -> Assert.True false
