let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Params = { namespace : List Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          "src/" ++ Prelude.Text.concatSep "/" params.namespace ++ ".hs"
      )
