let Algebra = ../Algebra.dhall

let Prelude = Algebra.Prelude

let Sdk = Algebra.Sdk

let Compiled = Sdk.Compiled

let ResultModule = ./Result.dhall

let Input = Algebra.Model.QueryFragments

let Output
    : Type
    = { exp : Text, haddock : Text }

let escapeText
    : Text -> Text
    = Prelude.Function.composeList
        Text
        [ Prelude.Text.replace "\"" "\\\""
        , Prelude.Text.replace "\\" "\\\\"
        , Prelude.Text.replace "\n" ("\\n\\" ++ "\n" ++ "\\")
        ]

let renderExp
    : Algebra.Model.QueryFragments -> Text
    = \(fragments : Algebra.Model.QueryFragments) ->
            "\""
        ++  Prelude.Text.concatMap
              Algebra.Model.QueryFragment
              ( \(queryFragment : Algebra.Model.QueryFragment) ->
                  merge
                    { Sql = escapeText
                    , Var =
                        \(var : Algebra.Model.QueryFragmentVar) ->
                              "\$"
                          ++  Algebra.Prelude.Natural.show (var.paramIndex + 1)
                    }
                    queryFragment
              )
              fragments
        ++  "\""

let renderHaddock
    : Algebra.Model.QueryFragments -> Text
    = Prelude.Text.concatMap
        Algebra.Model.QueryFragment
        ( \(queryFragment : Algebra.Model.QueryFragment) ->
            merge
              { Sql = Prelude.Function.identity Text
              , Var =
                  \(var : Algebra.Model.QueryFragmentVar) -> "\$" ++ var.rawName
              }
              queryFragment
        )

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        Compiled.ok
          Output
          { exp = renderExp input, haddock = renderHaddock input }

in  Algebra.module Input Output run
