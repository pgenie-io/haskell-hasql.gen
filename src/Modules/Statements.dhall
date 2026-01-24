let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Prelude = Prelude // { Text = Prelude.Text // Lude.Extensions.Text }

let Sdk = ../Sdk.dhall

let CodegenKit = ../CodegenKit.dhall

let Project = Sdk.Project

let Name = CodegenKit.Name

let Statement = ./Statement.dhall

let Snippets = ../Snippets/package.dhall

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

        in  { namespace
            , path = modulePath
            , content =
                Snippets.reexportModule
                  { namespace
                  , reexportedModules =
                      Prelude.List.map
                        Statement.Output
                        Text
                        (\(statement : Statement.Output) -> statement.namespace)
                        input.compiledStatementModules
                  }
            }

in  { Input, Output, compile }
