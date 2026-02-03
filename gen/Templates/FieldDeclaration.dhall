let Algebra = ./Algebra/package.dhall

let Params = { name : Text, sig : Text, docs : Optional Text }

in  Algebra.module
      Params
      ( \(params : Params) ->
          merge
            { Some =
                \(docs : Text) ->
                  let name = params.name

                  let sig = params.sig

                  in  ''
                      -- | ${docs}
                      ${name} :: ${sig}''
            , None =
                let name = params.name

                let sig = params.sig

                in  "${name} :: ${sig}"
            }
            params.docs
      )
