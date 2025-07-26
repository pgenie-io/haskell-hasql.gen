let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let TextExt = Lude.Extensions.Text

in  \(params : { namespace : Text, reexportedModules : List Text }) ->
      let importsBlock =
            Prelude.Text.concatMapSep
              "\n"
              Text
              (\(namespace : Text) -> "import ${namespace}")
              params.reexportedModules

      let exportsBlock =
            Prelude.Text.concatMapSep
              "\n"
              Text
              (\(statement : Text) -> "module ${statement},")
              params.reexportedModules

      in  ''
          module ${params.namespace} 
            ( ${TextExt.indent 4 exportsBlock}
            )
          where

          ${importsBlock}
          ''
