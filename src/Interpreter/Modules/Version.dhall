let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Model = Algebra.Model

let Input = Model.Project

let Output = List Sdk.File.Type

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Sdk.Compiled.ok Output ([] : Output)

in  Algebra.module Input Output run
