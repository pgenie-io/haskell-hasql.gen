let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Sdk = Deps.Sdk

let Model = Deps.Sdk.Project

let Input = Model.Project

let Output = List Sdk.File.Type

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.ok Output ([] : Output)

in  Algebra.module Input Output run
