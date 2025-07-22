let Prelude = ../Prelude.dhall

let Sdk = ../Sdk.dhall

let CodegenKit = ../CodegenKit.dhall

let Project = Sdk.Project

let Name = CodegenKit.Name

let Statement = ./Statement.dhall

let Input =
      { projectNamespace : Text
      , compiledStatementModules : List Statement.Output
      }

let Output = { namespace : Text, path : Text, content : Text }

let compile
    : Input -> Output
    = \(input : Input) ->
        let projectNamespace = input.projectNamespace

        let namespace = "${projectNamespace}.Statements"

        let modulePath = "${projectNamespace}/Statements.hs"

        let imports =
              Prelude.Text.concatMapSep
                "\n"
                Statement.Output
                ( \(statement : Statement.Output) ->
                    "import ${statement.namespace}"
                )
                input.compiledStatementModules

        let exports =
              Prelude.Text.concatMapSep
                ''
                ,
                ''
                Statement.Output
                ( \(statement : Statement.Output) ->
                    "module ${statement.namespace}"
                )
                input.compiledStatementModules

        in  { namespace
            , path = modulePath
            , content =
                ''
                module ${namespace} 
                  ( ${exports}
                  )
                where

                ${imports}
                ''
            }

in  { Input, Output, compile }
