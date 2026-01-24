let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Typeclasses = ../Typeclasses.dhall

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

let templateModule =
      \(Params : Type) ->
        let Run = Params -> Text in \(run : Run) -> { Params, Run, run }

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
    , Snippets
    , Import
    , Import/render
    , Imports
    , module
    , templateModule
    }
