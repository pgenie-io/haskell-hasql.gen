let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = Optional Text

in  Algebra.module
      Params
      ( \(params : Params) ->
          merge
            { None = ""
            , Some =
                \(unprefixedText : Text) ->
                      "-- | "
                  ++  Deps.Lude.Extensions.Text.prefixEachLine
                        "-- "
                        unprefixedText
                  ++  "\n"
            }
            params
      )
