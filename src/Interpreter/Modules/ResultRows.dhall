let Algebra = ../Algebra.dhall

let Member = ./Member.dhall

let Input = Algebra.Model.ResultRows

let Output =
      Text -> { decoderExp : Text, rowTypeDecl : Text, resultTypeDecl : Text }

let Result = Algebra.Sdk.Compiled.Type Output

let run
    : Input -> Result
    = \(input : Input) ->
        let compiledColumns =
              Algebra.Typeclasses.Classes.Applicative.traverseList
                Algebra.Sdk.Compiled.Type
                Algebra.Sdk.Compiled.applicative
                Algebra.Model.Member
                Member.Output
                Member.run
                ( Algebra.Prelude.NonEmpty.toList
                    Algebra.Model.Member
                    input.columns
                )

        in  Algebra.Sdk.Compiled.flatMap
              (List Member.Output)
              Output
              ( \(columns : List Member.Output) ->
                  Algebra.Sdk.Compiled.ok
                    Output
                    ( \(typeNameBase : Text) ->
                        let rowTypeName = "${typeNameBase}ResultRow"

                        let rowTypeDecl =
                              Algebra.Snippets.recordDataDecl
                                { name = rowTypeName
                                , fields =
                                    Algebra.Prelude.List.map
                                      Member.Output
                                      { name : Text, sig : Text }
                                      ( \(column : Member.Output) ->
                                          { name = column.fieldName
                                          , sig = column.sig
                                          }
                                      )
                                      columns
                                }

                        let rowDecoderExp =
                              ''
                              do
                                ${Algebra.Lude.Extensions.Text.indent
                                    2
                                    ( Algebra.Prelude.Text.concatMap
                                        Member.Output
                                        ( \(column : Member.Output) ->
                                            ''
                                            ${column.fieldName} <- Decoders.column (${column.decoderExp})
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
