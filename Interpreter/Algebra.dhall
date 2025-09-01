let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let CodegenKit = ../CodegenKit.dhall

let Prelude = ../Prelude.dhall

let Snippets = ../Snippets/package.dhall

let module =
      \(Input : Type) ->
      \(Output : Type) ->
        let Result = Sdk.Compiled.Type Output

        let Run = Input -> Result

        in  \(run : Run) -> { Input, Output, Result, Run, run }

let Name = CodegenKit.Name

let Import = < HasqlDecoders | HasqlEncoders | HasqlStatement >

let Import/render =
      \(import : Import) ->
        merge
          { HasqlDecoders = "Hasql.Decoders"
          , HasqlEncoders = "Hasql.Encoders"
          , HasqlStatement = "Hasql.Statement"
          }
          import

let Imports =
      let Self =
            { hasqlDecoders : Bool
            , hasqlEncoders : Bool
            , hasqlStatement : Bool
            }

      let default
          : Self
          = { hasqlDecoders = False
            , hasqlEncoders = False
            , hasqlStatement = False
            }

      let render
          : Self -> Text
          = \(imports : Self) -> ""

      in  { Type = Self, default, render }

in  { Lude
    , Sdk
    , Prelude
    , Name
    , Model
    , Snippets
    , module
    , Import
    , Import/render
    , Imports
    }
