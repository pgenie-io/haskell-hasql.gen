let Algebra = ./Algebra/package.dhall

let Params = {}

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          package: .

          source-repository-package
            type: git
            location: https://github.com/nikita-volkov/hasql-mapping
            tag: 14831e9cdea46b8b23148f4cd5f77d2a0bf81239
          ''
      )
