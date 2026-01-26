let Algebra = ./Algebra/package.dhall

let Prelude = ../Prelude.dhall

let Params = { namespace : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          "src/" ++ Prelude.Text.concatSep "/" params.namespace ++ ".hs"
      )
