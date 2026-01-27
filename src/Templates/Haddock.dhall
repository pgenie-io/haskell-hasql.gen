let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params = { isDoc : Bool, text : Optional Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          merge
            { None = ""
            , Some =
                \(text : Text) ->
                      (if params.isDoc then "-- | " else "-- ")
                  ++  Deps.Lude.Extensions.Text.prefixEachLine "-- " text
                  ++  "\n"
            }
            params.text
      )
