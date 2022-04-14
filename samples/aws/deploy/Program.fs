module Primacy.Symfig.Samples.Aws.Deploy

open Amazon.CDK
open Amazon.CDK.AWS
open Amazon.CDK.AWS.ECS
open Amazon.CDK.AWS.ECS.Patterns

open Primacy.Symfig

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
      envvars |> Config.EnvVars.choose (function _, PlainText v -> Some v | _ -> None),
      envvars |> Config.EnvVars.choose (function _, Secret v -> Some v | _ -> None)
    | Error error ->
      invalidOp $"Configuration error.\n{Config.EnvErrors.print error}"

  let _ =
    ApplicationLoadBalancedFargateService(stack, "MyApplication",
      ApplicationLoadBalancedFargateServiceProps(
        TaskImageOptions =
          ApplicationLoadBalancedTaskImageOptions(
            // Root of the repo so we can copy everything.
            Image = ContainerImage.FromAsset ("../../../",
              AssetImageProps (File = "samples/aws/src/Dockerfile")
            ),
            Environment = environment,
            Secrets = secrets
          )
      )
    )

  0
