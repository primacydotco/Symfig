module Primacy.Symfig.Samples.Aws

open Primacy.Symfig

/// Application configuration module.
// Declare a configuration module like this in shared code.
// In this sample, deploy.fsproj depends upon src.fsproj so this module is available to both projects.
// You can also declare this in a way your configuration module doesn't have a dependency on Primacy.Symfig. e.g.
// module Config =
//   type Environment<'value> = {
//     Says : 'value
//     Reveals : 'value
//   }
//   let Prefix = None
//   let Append = fun a b -> $"{a}__{b}"
module Config =
  /// A type to represent our application's configuration.
  // It uses a type parameter for it's values so the type of values written can be different to the type of values read.
  // In this sample, we write strings or secrets (see the ConfigValue type in ../deploy/Progra.fs) and read strings.
  type T<'value> = {
    Says : 'value
    Reveals : 'value
  }

  let options : Config.KeyOptions = {
    // A prefix for environment variable keys.
    Prefix = None
    // A function to build environment variable keys from nested properties.
    Append = fun a b -> $"{a}__{b}"
  }

[<EntryPoint>]
let main args =
  /// A function which fetches an enviroment variable (or none).
  let env key =
    System.Environment.GetEnvironmentVariable key
    |> Option.ofObj
    |> Option.filter (not << System.String.IsNullOrWhiteSpace)

  // The configuration read from the environment (or errors).
  let config = Config.read<string, Config.T<string>> [ ] Config.options env

  match config with
  | Ok config ->
    printfn $"{config.Says}. '{config.Reveals}'"
  | Error e ->
    printfn $"Configuration error.\n{Config.EnvErrors.print e}"

  0

