let Algebra = ../Algebra.dhall

let Member = ./Member.dhall

let Input = Algebra.Model.ResultRows

let Params = { typeNameBase : Text }

let Output = { decoderExp : Text, rowTypeDecl : Text, resultTypeDecl : Text }

let Error = < Column : Member.Error | Todo >

let Result = Algebra.Lude.Structures.Result.Type Error Output

let run
    : Input -> Params -> Result
    = \(input : Input) ->
      \(params : Params) ->
        let compiledColumns =
              Algebra.Lude.Structures.Result.traverseList
                Error
                Algebra.Model.Member
                Member.Output
                ( \(column : Algebra.Model.Member) ->
                    Algebra.Lude.Structures.Result.mapError
                      Member.Error
                      Error
                      Member.Output
                      (\(error : Member.Error) -> Error.Column error)
                      (Member.run column)
                )
                ( Algebra.Prelude.NonEmpty.toList
                    Algebra.Model.Member
                    input.columns
                )

        in  Algebra.Lude.Structures.Result.flatMap
              Error
              (List Member.Output)
              Output
              ( \(columns : List Member.Output) ->
                  let rowTypeName = "${params.typeNameBase}ResultRow"

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
                                    "${column.fieldName} <- ${column.decoderExp}"
                                )
                                columns
                            )}                           
                          pure ${rowTypeName} {..}''

                  let output =
                        merge
                          { Optional =
                            { decoderExp = "Decoder.rowMaybe ${rowDecoderExp}"
                            , rowTypeDecl
                            , resultTypeDecl =
                                "type ${params.typeNameBase}Result = Maybe ${rowTypeName}"
                            }
                          , Single =
                            { decoderExp = "Decoder.singleRow ${rowDecoderExp}"
                            , rowTypeDecl
                            , resultTypeDecl =
                                "type ${params.typeNameBase}Result = ${rowTypeName}"
                            }
                          , Multiple =
                            { decoderExp = "Decoder.rowVector ${rowDecoderExp}"
                            , rowTypeDecl
                            , resultTypeDecl =
                                "type ${params.typeNameBase}Result = Vector.Vector ${rowTypeName}"
                            }
                          }
                          input.cardinality

                  in  Result.Success output
              )
              compiledColumns

in  Algebra.parametricModule Input Params Output Error run
