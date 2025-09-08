let Algebra = ../Algebra.dhall

let Prelude = Algebra.Prelude

let Sdk = Algebra.Sdk

let Compiled = Sdk.Compiled

let ResultModule = ./Result.dhall

let Input = Algebra.Model.QueryFragments

let Output
    : Type
    = { exp : Text }

let renderSql
    : Text -> Text
    = \(text : Text) ->
        Prelude.Text.replace "\\" "\\\\" (Prelude.Text.replace "\"" "\\\"" text)

let run
    : Input -> Compiled.Type Output
    = \(input : Input) -> Compiled.message Output "TODO"

in  Algebra.module Input Output run
