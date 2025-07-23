let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let order = ./order.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let separate = ./separate.dhall

let squash = ./squash.dhall

let singleton = ./singleton.dhall

let word = ./word.dhall

let namespace1 =
      separate
        (word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ])
        [ word LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ] ]

let namespace2 =
      separate
        ( word
            LatinChar.C
            [ LatinChar.O
            , LatinChar.N
            , LatinChar.T
            , LatinChar.R
            , LatinChar.O
            , LatinChar.L
            ]
        )
        [ word
            LatinChar.M
            [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
        ]

let namespace3 =
      separate
        (word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ])
        [ word LatinChar.M [ LatinChar.A, LatinChar.P ] ]

let singleNamespace =
      word
        LatinChar.P
        [ LatinChar.R
        , LatinChar.E
        , LatinChar.L
        , LatinChar.U
        , LatinChar.D
        , LatinChar.E
        ]

in  { singleElementNamespaces =
      { sameNamespace =
            assert
          :     order.compare singleNamespace singleNamespace
            ===  Lude.Algebras.Order.Comparison.Equal
      , differentNamespaces =
          let namespace4 =
                word LatinChar.M [ LatinChar.A, LatinChar.I, LatinChar.N ]

          in    assert
              :     order.compare singleNamespace namespace4
                ===  Lude.Algebras.Order.Comparison.Greater
      }
    , multiElementNamespaces =
      { sameNamespaces =
            assert
          :     order.compare namespace1 namespace1
            ===  Lude.Algebras.Order.Comparison.Equal
      , differentByHead =
          -- "Control" < "Data"
            assert
          :     order.compare namespace2 namespace1
            ===  Lude.Algebras.Order.Comparison.Smaller
      , sameHeadDifferentTail =
          -- Data.List vs Data.Map
            assert
          :     order.compare namespace1 namespace3
            ===  Lude.Algebras.Order.Comparison.Smaller
      }
    , differentLengths =
      { shorterVsLonger =
          let shortNamespace =
                word LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]

          in    assert
              :     order.compare shortNamespace namespace1
                ===  Lude.Algebras.Order.Comparison.Smaller
      , longerVsShorter =
          let longNamespace =
                separate
                  ( word
                      LatinChar.C
                      [ LatinChar.O
                      , LatinChar.N
                      , LatinChar.T
                      , LatinChar.R
                      , LatinChar.O
                      , LatinChar.L
                      ]
                  )
                  [ word
                      LatinChar.M
                      [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
                  , word
                      LatinChar.R
                      [ LatinChar.E
                      , LatinChar.A
                      , LatinChar.D
                      , LatinChar.E
                      , LatinChar.R
                      ]
                  ]

          in    assert
              :     order.compare longNamespace namespace2
                ===  Lude.Algebras.Order.Comparison.Greater
      }
    }
