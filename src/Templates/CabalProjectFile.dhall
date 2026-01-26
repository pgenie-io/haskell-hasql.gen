let Algebra = ../Algebras/Template/package.dhall

let Params = {}

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          package: .

          source-repository-package
            type: git
            location: https://github.com/nikita-volkov/hasql-mapping
            tag: 700db9ca5a0bebb1016d8100a707d61f68455458
          ''
      )
