let Prelude = ../Prelude.dhall

in  \(text : Text) ->
      let escapedText =
            Prelude.Text.replace
              (Prelude.Text.replace text "\"" "\\\"")
              "\n"
              "\\n"

      in  "\"" ++ escapedText ++ "\""
