let Algebra = ../Algebra.dhall

let Sdk = Algebra.Sdk

let Lude = Algebra.Lude

let Model = Algebra.Model

let Input = Model.CustomType

let Output = {}

let run = \(input : Input) -> Sdk.Compiled.ok Output {=}

in  Algebra.module Input Output run
