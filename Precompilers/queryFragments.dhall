let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let Snippets = ../Snippets/package.dhall

let CodegenKit = ../CodegenKit.dhall

let Name = CodegenKit.Name

in  \(fragments : List Model.QueryFragment) ->
      let Acc =
            { sql : Text
            , -- Deduplicated names in reverted order of the associated numeric placeholders.
              names : List Name.Type
            , placeholderOffset : Natural
            }

      let acc
          : Acc
          = Prelude.List.foldLeft
              Model.QueryFragment
              fragments
              Acc
              ( \(acc : Acc) ->
                \(fragment : Model.QueryFragment) ->
                  merge
                    { Sql = \(sql : Text) -> acc // { sql = acc.sql ++ sql }
                    , Var =
                        \(name : Name.Type) ->
                          if    Lude.Extensions.List.elem
                                  Name.Type
                                  Name.equality
                                  name
                                  acc.names
                          then  acc
                          else  let placeholderOffset =
                                      acc.placeholderOffset + 1

                                let names
                                    : List Name.Type
                                    = [ name ] # acc.names

                                let sql =
                                          "\$"
                                      ++  Prelude.Natural.show placeholderOffset

                                in  { sql, names, placeholderOffset }
                    }
                    fragment
              )
              { sql = ""
              , names = Prelude.List.empty Name.Type
              , placeholderOffset = 0
              }

      let stringLiteral
          : Text
          = Snippets.stringLiteral acc.sql

      let names
          : List Name.Type
          = Prelude.List.reverse Name.Type acc.names

      in  { stringLiteral, names }
