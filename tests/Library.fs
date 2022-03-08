module Primacy.Symfig.Library

open System
open Xunit

// module private Validation =
//   let force (v : Result<_,_>) =
//     match v with
//     | Ok r -> r
//     | Error e -> invalidOp $"{e}"

// type SampleRecordA<'v> = {
//   TEST_1 : SampleRecordB<'v>
//   TEST_2 : 'v
//   TEST_3 : SampleRecordB<'v> * SampleRecordC<'v>
//   TEST_4 : 'v
//   TEST_5 : 'v list
//   TEST_6 : 'v option
//   TEST_7 : 'v option
//   TEST_8 : SampleRecordC<'v> option
//   TEST_9 : SampleRecordC<'v> option
//   TEST_10 : {|
//     PROPERTY_X : 'v
//     PROPERTY_Y : 'v
//     PROPERTY_Z : {|
//       PROPERTY_F : 'v
//     |}
//   |}
// }

// and SampleRecordB<'v> = {
//   PROPERTY_1 : 'v
//   PROPERTY_2 : 'v
// }
// and SampleRecordC<'v> = {
//   PROPERTY_3 : 'v
// }

// let sampleKeyOptions : KeyOptions = {
//   Prefix = Some "MY"
//   Append = fun a b -> $"{a}__{b}"
// }

// let sampleRecordA = {
//   TEST_1 = {
//     PROPERTY_1 = "1"
//     PROPERTY_2 = "2"
//   }
//   TEST_2 = "X"
//   TEST_3 = {
//     PROPERTY_1 = "1"
//     PROPERTY_2 = "2"
//     }, {
//     PROPERTY_3 = "3"
//     }
//   TEST_4 = "true"
//   TEST_5 = [ "a"; "b"; "c" ]
//   TEST_6 = None
//   TEST_7 = Some "bloop"
//   TEST_8 = None
//   TEST_9 = Some {
//     PROPERTY_3 = "3"
//   }
//   TEST_10 = {|
//     PROPERTY_X = "x"
//     PROPERTY_Y = "y"
//     PROPERTY_Z = {|
//       PROPERTY_F = "f"
//     |}
//   |}
// }

// let sampleVariablesA = [
//   "MY__TEST_1__PROPERTY_1", "1"
//   "MY__TEST_1__PROPERTY_2", "2"
//   "MY__TEST_2", "X"
//   "MY__TEST_3__PROPERTY_1", "1"
//   "MY__TEST_3__PROPERTY_2", "2"
//   "MY__TEST_3__PROPERTY_3", "3"
//   "MY__TEST_4", "true"
//   "MY__TEST_5", "a;b;c"
//   //"MY__TEST_6", is none
//   "MY__TEST_7", "bloop"
//   //"MY__TEST_8__PROPERTY_3", is none
//   "MY__TEST_9__PROPERTY_3", "3"
//   "MY__TEST_10__PROPERTY_X", "x"
//   "MY__TEST_10__PROPERTY_Y", "y"
//   "MY__TEST_10__PROPERTY_Z__PROPERTY_F", "f"
// ]

// let sampleEnv key =
//   sampleVariablesA
//   |> List.tryFind (fun (k,_) -> k = key)
//   |> Option.map snd

// [<Fact>]
// let ``write`` () =
//   let result =
//     Library.Strings.write sampleKeyOptions sampleRecordA

//   match result with
//   | Ok result ->
//     Assert.Equal<(string * string) list> (sampleVariablesA, result)
//   | Error e ->
//     invalidOp $"%A{e}"

// [<Fact>]
// let ``read`` () =
//   let result =
//     Validation.force <| Strings.read<SampleRecordA<string>> sampleKeyOptions sampleEnv

//   Assert.Equal (sampleRecordA, result)

// [<Fact>]
// let ``read optional`` () =

//   let result =
//     Validation.force <| Library.Strings.read<SampleRecordA<string> option> sampleKeyOptions (fun _ -> None)

//   Assert.Equal (None, result)

// [<Fact>]
// let ``read non-optional returns error`` () =
//   let target = Library.Strings.read<{| A : string; B: {| Y: string; Z: string |}; C: string option |}>
//   let targetNonOptional = 3

//   match target sampleKeyOptions (fun _ -> None) with
//   | Ok _ -> Assert.True false
//   | Error e -> Assert.Equal (targetNonOptional, e.Length)