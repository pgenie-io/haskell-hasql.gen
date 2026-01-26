let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = { namespace : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          "src/" ++ Deps.Prelude.Text.concatSep "/" params.namespace ++ ".hs"
      )
