module Primacy.Symfig.Tests

open System
open Xunit

module private Validation =
  let force (v : Result<_,_>) =
    match v with
    | Ok r -> r
    | Error e -> invalidOp $"{e}"

type SampleRecordA = {
  TEST_1 : SampleRecordB
  TEST_2 : string
  TEST_3 : SampleRecordB * SampleRecordC
  TEST_4 : bool
  TEST_5 : string list
  TEST_6 : string option
  TEST_7 : string option
  TEST_8 : SampleRecordC option
  TEST_9 : SampleRecordC option
  TEST_10 : {|
    PROPERTY_X : string
    PROPERTY_Y : string
    PROPERTY_Z : {|
      PROPERTY_F : string
    |}
  |}
}
and SampleRecordB = {
  PROPERTY_1 : string
  PROPERTY_2 : string
}
and SampleRecordC = {
  PROPERTY_3 : string
}

let samplePrefix = "MY"

let sampleRecord = {
  TEST_1 = {
    PROPERTY_1 = "1"
    PROPERTY_2 = "2"
  }
  TEST_2 = "X"
  TEST_3 = {
    PROPERTY_1 = "1"
    PROPERTY_2 = "2"
    }, {
    PROPERTY_3 = "3"
    }
  TEST_4 = true
  TEST_5 = [ "a"; "b"; "c" ]
  TEST_6 = None
  TEST_7 = Some "bloop"
  TEST_8 = None
  TEST_9 = Some {
    PROPERTY_3 = "3"
  }
  TEST_10 = {|
    PROPERTY_X = "x"
    PROPERTY_Y = "y"
    PROPERTY_Z = {|
      PROPERTY_F = "f"
    |}
  |}
}

let sampleVariables = [
  "MY__TEST_1__PROPERTY_1", "1"
  "MY__TEST_1__PROPERTY_2", "2"
  "MY__TEST_2", "X"
  "MY__TEST_3__PROPERTY_1", "1"
  "MY__TEST_3__PROPERTY_2", "2"
  "MY__TEST_3__PROPERTY_3", "3"
  "MY__TEST_4", "true"
  "MY__TEST_5", "a;b;c"
  //"MY__TEST_6", is none
  "MY__TEST_7", "bloop"
  //"MY__TEST_8__PROPERTY_3", is none
  "MY__TEST_9__PROPERTY_3", "3"
  "MY__TEST_10__PROPERTY_X", "x"
  "MY__TEST_10__PROPERTY_Y", "y"
  "MY__TEST_10__PROPERTY_Z__PROPERTY_F", "f"
]

let sampleEnv key =
  sampleVariables
  |> List.tryFind (fun (k,_) -> k = key)
  |> Option.map snd

[<Fact>]
let ``write`` () =
  let result =
    Library.write samplePrefix sampleRecord

  Assert.Equal<(string * string) seq> (sampleVariables, result)

[<Fact>]
let ``read`` () =
  let result =
    Validation.force <| Library.read<SampleRecordA> samplePrefix sampleEnv

  Assert.Equal (sampleRecord, result)

[<Fact>]
let ``read optional`` () =

  let result =
    Validation.force <|  Library.read<SampleRecordA option> samplePrefix (fun _ -> None)

  Assert.Equal (None, result)

[<Fact>]
let ``read non-optional returns error`` () =
  let target = Library.read<{| A : string; B: {| Y: string; Z: string |}; C: string option |}>
  let targetNonOptional = 3

  match target samplePrefix (fun _ -> None) with
  | Ok _ -> Assert.True false
  | Error e -> Assert.Equal (targetNonOptional, e.Length)
