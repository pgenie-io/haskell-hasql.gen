let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Link = { target : Text, label : Text }

let Link/render =
      \(link : Link) -> "[" ++ link.label ++ "](" ++ link.target ++ ")"

let Node = < Link : Link | Text : Text >

let Node/render =
      \(node : Node) ->
        merge { Link = Link/render, Text = \(text : Text) -> text } node

let Params = List Node

in      Algebra.module
          Params
          ( \(params : Params) ->
              if    Deps.Prelude.List.null Node params
              then  ""
              else      ''
                        -- |
                        ''
                    ++  Deps.Lude.Extensions.Text.prefixEachLine
                          "-- "
                          ( Deps.Prelude.List.fold
                              Node
                              params
                              Text
                              ( \(node : Node) ->
                                \(acc : Text) ->
                                  acc ++ Node/render node
                              )
                              ""
                          )
          )
    //  { Link, Node }
