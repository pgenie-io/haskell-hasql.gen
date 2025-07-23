let Prelude = ../../Prelude.dhall

let Lude = ../../Lude.dhall

let CodegenKit = ../../CodegenKit.dhall

let order = ./order.dhall

let NamespaceType = ./Type.dhall

let Name = CodegenKit.Name

let LatinChars = Lude.Structures.LatinChars

let LatinChar = LatinChars.Char.Type

let singleWordName =
      \(head : LatinChars.Char.Type) ->
      \(tail : List LatinChars.Char.Type) ->
        Name.fromLatinChars (LatinChars.fromHeadAndTail head tail)

let namespace1 =
        { head =
            singleWordName LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]
        , tail =
          [ singleWordName LatinChar.L [ LatinChar.I, LatinChar.S, LatinChar.T ]
          ]
        }
      : NamespaceType

let namespace2 =
        { head =
            singleWordName
              LatinChar.C
              [ LatinChar.O
              , LatinChar.N
              , LatinChar.T
              , LatinChar.R
              , LatinChar.O
              , LatinChar.L
              ]
        , tail =
          [ singleWordName
              LatinChar.M
              [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
          ]
        }
      : NamespaceType

let namespace3 =
        { head =
            singleWordName LatinChar.D [ LatinChar.A, LatinChar.T, LatinChar.A ]
        , tail = [ singleWordName LatinChar.M [ LatinChar.A, LatinChar.P ] ]
        }
      : NamespaceType

let singleNamespace =
        { head =
            singleWordName
              LatinChar.P
              [ LatinChar.R
              , LatinChar.E
              , LatinChar.L
              , LatinChar.U
              , LatinChar.D
              , LatinChar.E
              ]
        , tail = [] : List Name.Type
        }
      : NamespaceType

in  { orderFunction =
      { singleElementNamespaces =
        { sameNamespace =
              assert
            :     order.compare singleNamespace singleNamespace
              ===  Lude.Algebras.Order.Comparison.Equal
        , differentNamespaces =
            let namespace4 =
                    { head =
                        singleWordName
                          LatinChar.M
                          [ LatinChar.A, LatinChar.I, LatinChar.N ]
                    , tail = [] : List Name.Type
                    }
                  : NamespaceType

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
                    { head =
                        singleWordName
                          LatinChar.D
                          [ LatinChar.A, LatinChar.T, LatinChar.A ]
                    , tail = [] : List Name.Type
                    }
                  : NamespaceType

            in    assert
                :     order.compare shortNamespace namespace1
                  ===  Lude.Algebras.Order.Comparison.Smaller
        , longerVsShorter =
            let longNamespace =
                    { head =
                        singleWordName
                          LatinChar.C
                          [ LatinChar.O
                          , LatinChar.N
                          , LatinChar.T
                          , LatinChar.R
                          , LatinChar.O
                          , LatinChar.L
                          ]
                    , tail =
                      [ singleWordName
                          LatinChar.M
                          [ LatinChar.O, LatinChar.N, LatinChar.A, LatinChar.D ]
                      , singleWordName
                          LatinChar.R
                          [ LatinChar.E
                          , LatinChar.A
                          , LatinChar.D
                          , LatinChar.E
                          , LatinChar.R
                          ]
                      ]
                    }
                  : NamespaceType

            in    assert
                :     order.compare longNamespace namespace2
                  ===  Lude.Algebras.Order.Comparison.Greater
        }
      }
    }
