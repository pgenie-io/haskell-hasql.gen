let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Prelude = Prelude // { Text = Prelude.Text // Lude.Extensions.Text }

let Field = { name : Text, sig : Text }

in  \(params : { name : Text, fields : List Field }) ->
      ''
      data ${params.name} = ${params.name}
        { ${Prelude.Text.indent
              4
              ( Prelude.Text.concatMapSep
                  ''
                  ,
                  ''
                  Field
                  (\(field : Field) -> "${field.name} :: ${field.sig}")
                  params.fields
              )}
        }''
