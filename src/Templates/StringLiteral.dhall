let Algebra = ./Algebra/package.dhall

let Prelude = ../Prelude.dhall

let Params = Text

in  Algebra.module
      Params
      ( \(params : Params) ->
              "\""
          ++  Prelude.Function.composeList
                Text
                [ Prelude.Text.replace "\"" "\\\""
                , Prelude.Text.replace "\\" "\\\\"
                , Prelude.Text.replace "\n" ("\\n\\" ++ "\n" ++ "\\")
                ]
                params
          ++  "\""
      )
