let Lude = ../Lude.dhall

let Result = Lude.Structures.Result

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let CodegenKit = ../CodegenKit.dhall

let Prelude = ../Prelude.dhall

let Snippets = ../Snippets/package.dhall

let module =
      \(Input : Type) ->
      \(Output : Type) ->
      \(Error : Type) ->
        let Result = Result.Type Error Output

        let Run = Input -> Result

        in  \(run : Run) -> { Input, Output, Error, Result, Run, run }

let parametricModule =
      \(Input : Type) ->
      \(Params : Type) ->
      \(Output : Type) ->
      \(Error : Type) ->
        let Result = Result.Type Error Output

        let Run = Input -> Params -> Result

        in  \(run : Run) -> { Input, Params, Output, Error, Result, Run, run }

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
    , parametricModule
    , Import
    , Import/render
    , Imports
    }
