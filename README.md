# Symfig

[![NuGet version (Primacy.Symfig)](https://img.shields.io/nuget/vpre/Primacy.Symfig?style=flat-square)

Symfig is a library for reliable configuration as code.

We write our applications and define our infrastructure in F#. Why not config too?
Unlike [FsConfig](https://github.com/demystifyfp/FsConfig), Symfig lets you read and _write_ environment variables so that you'll never have runtime errors because of misnamed or missed environment variables.

**Misnamed** environment variables might occur where the application reads an environment variable named `LICENSE_KEY` but the environment declares one named `LICENCE_KEY`.

**Missed** environment variables might occur where the application starts depending on a new environment variable, but it never gets declared in the environment.

## Usage

### Basic usage

```fsharp
open Primacy.Symfig

type SampleConfig = {
  ConfigA : string
  ConfigB : {|
    Config1 : string
    Config2 : bool
  |}
  ConfigC : string option
}

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

let find (key : string) : string option =
  match values with
  | Ok values -> values |> Map.tryFind key
  | Error _ -> None

let config' = Config.String.read<SampleConfig> options find

Ok config = config'
```

### AWS CDK usage

```fsharp
open Primacy.Symfig

/// Application configuration module.
// Declare a configuration module like this in shared code.
// You can also declare this in a way your configuration module doesn't have a dependency on Primacy.Symfig.
module Config =
  /// A type to represent our application's configuration.
  // It uses a type parameter for it's values so the type of values written can be different to the type of values read.
  type T<'value> = {
    Says : 'value
    Reveals : 'value
  }

  let options = {
    // A prefix for environment variable keys.
    Prefix = None
    // A function to build environment variable keys from nested properties.
    Append = fun a b -> $"{a}__{b}"
  }

module App =
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
      printfn $"Configuration error.\n{EnvErrors.print e}"

    0

module Deploy =
  open Amazon.CDK
  open Amazon.CDK.AWS
  open Amazon.CDK.AWS.ECS
  open Amazon.CDK.AWS.ECS.Patterns

  /// A type which represents the two kinds of configuration we use: plain text and secrets.
  type ConfigValue =
    | PlainText of string
    | Secret of Secret

  [<EntryPoint>]
  let main args =
    let app = App ()
    let stack = Stack (app, "MyStack")

    /// Our configuration written with its keys and values (or an error).
    let config =
      Config.write<ConfigValue, Config.T<ConfigValue>> [ ] Config.options {
        Says = PlainText "Hello"
        Reveals =
          SecretsManager.Secret.FromSecretNameV2 (stack, "MySecret", "MySecretName")
          |> Secret.FromSecretsManager
          |> Secret
      }

    /// Our configuration values partitioned into plain text (regular environment variables) and secrets (ECS secrets).
    let environment, secrets =
      match config with
      | Ok envvars ->
        envvars |> EnvVars.choose (function _, PlainText v -> Some v | _ -> None),
        envvars |> EnvVars.choose (function _, Secret v -> Some v | _ -> None)
      | Error error ->
        invalidOp $"Configuration error.\n{EnvErrors.print error}"

    let _ =
      ApplicationLoadBalancedFargateService(stack, "MyApplication",
        ApplicationLoadBalancedFargateServiceProps(
          TaskImageOptions =
            ApplicationLoadBalancedTaskImageOptions(
              Image = ContainerImage.FromAsset ("../../../",
                AssetImageProps (File = "samples/aws/src/Dockerfile")
              ),
              // Adds plain text configuration values
              Environment = environment,
              // Adds secret configuration values
              Secrets = secrets
            )
        )
      )

    0

```
