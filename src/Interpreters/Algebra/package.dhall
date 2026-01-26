let Deps = ../../Deps/package.dhall

let Lude = Deps.Lude

let Sdk = Deps.Sdk

let Typeclasses = Deps.Typeclasses

let Model = Sdk.Project

let CodegenKit = Deps.CodegenKit

let Prelude = Deps.Prelude

let Config = { rootNamespace : List Text }

let module =
      \(Input : Type) ->
      \(Output : Type) ->
        let Result = Sdk.Compiled.Type Output

        let Run = Config -> Input -> Result

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

in  { Typeclasses
    , Lude
    , Sdk
    , Prelude
    , Name
    , Model
    , Import
    , Import/render
    , Imports
    , Config
    , module
    }
