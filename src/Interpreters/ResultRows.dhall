let Deps = ../Deps/package.dhall

let Algebra = ./Algebra/package.dhall

let Templates = ../Templates/package.dhall

let Member = ./Member.dhall

let Input = Deps.Sdk.Project.ResultRows

let Output =
      Text -> { decoderExp : Text, rowTypeDecl : Text, resultTypeDecl : Text }

let run =
      \(config : Algebra.Config) ->
      \(input : Input) ->
        let compiledColumns =
              Deps.Typeclasses.Classes.Applicative.traverseList
                Deps.Sdk.Compiled.Type
                Deps.Sdk.Compiled.applicative
                Deps.Sdk.Project.Member
                Member.Output
                (Member.run config)
                ( Deps.Prelude.NonEmpty.toList
                    Deps.Sdk.Project.Member
                    input.columns
                )

        in  Deps.Sdk.Compiled.flatMap
              (List Member.Output)
              Output
              ( \(columns : List Member.Output) ->
                  Deps.Sdk.Compiled.ok
                    Output
                    ( \(typeNameBase : Text) ->
                        let rowTypeName = "${typeNameBase}ResultRow"

                        let rowTypeDecl =
                              Templates.RecordDeclaration.run
                                { name = rowTypeName
                                , fields =
                                    Deps.Prelude.List.map
                                      Member.Output
                                      Text
                                      ( \(column : Member.Output) ->
                                          column.fieldDeclaration
                                      )
                                      columns
                                }

                        let rowDecoderExp =
                              ''
                              do
                                ${Deps.Lude.Extensions.Text.indent
                                    2
                                    ( Deps.Prelude.Text.concatMap
                                        Member.Output
                                        ( \(column : Member.Output) ->
                                            ''
                                            ${column.fieldName} <- Decoders.column (${column.fieldDecoder})
                                            ''
                                        )
                                        columns
                                    )}pure ${rowTypeName} {..}''

                        let output =
                              merge
                                { Optional =
                                  { decoderExp =
                                      "Decoders.rowMaybe ${rowDecoderExp}"
                                  , rowTypeDecl
                                  , resultTypeDecl =
                                      "type ${typeNameBase}Result = Maybe ${rowTypeName}"
                                  }
                                , Single =
                                  { decoderExp =
                                      "Decoders.singleRow ${rowDecoderExp}"
                                  , rowTypeDecl
                                  , resultTypeDecl =
                                      "type ${typeNameBase}Result = ${rowTypeName}"
                                  }
                                , Multiple =
                                  { decoderExp =
                                      "Decoders.rowVector ${rowDecoderExp}"
                                  , rowTypeDecl
                                  , resultTypeDecl =
                                      "type ${typeNameBase}Result = Vector.Vector ${rowTypeName}"
                                  }
                                }
                                input.cardinality

                        in  output
                    )
              )
              compiledColumns

in  Algebra.module Input Output run
