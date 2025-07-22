let Sdk = ../Sdk.dhall

let CodegenKit = ../CodegenKit.dhall

let Project = Sdk.Project

let Name = CodegenKit.Name

let Input = { projectNamespace : Text, query : Project.Query }

let Output = { namespace : Text, path : Text, content : Text }

let compile
    : Input -> Output
    = \(input : Input) ->
        let projectNamespace = input.projectNamespace

        let moduleName = Name.toTextInPascal input.query.name

        let namespace = "${projectNamespace}.Statements.${moduleName}"

        let modulePath = "${projectNamespace}/Statements/${moduleName}.hs"

        in  { namespace
            , path = modulePath
            , content =
                ''
                module ${namespace} where

                import Prelude
                import qualified Hasql.Statement as Statement

                -- TODO
                ''
            }

in  { Input, Output, compile }
