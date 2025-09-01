let Lude = ../Lude.dhall

let Sdk = ../Sdk.dhall

let Model = Sdk.Project

let CodegenKit = ../CodegenKit.dhall

let Prelude = ../Prelude.dhall

let Snippets = ../Snippets/package.dhall

let Error = { path : List Text, message : Text }

let Error/nest =
      \(pathSegment : Text) ->
      \(error : Error) ->
        error // { path = [ pathSegment ] # error.path }

let Error/message =
      \(message : Text) -> { path = [] : List Text, message } : Error

let Result = Lude.Structures.Result.Type Error

let Result/nest =
      \(A : Type) ->
      \(context : Text) ->
      \(result : Result A) ->
          Lude.Structures.Result.mapError
            Error
            Error
            A
            (Error/nest context)
            result
        : Result A

let Result/message =
      \(A : Type) ->
      \(message : Text) ->
        (Result A).Failure (Error/message message) : Result A

let Result/map =
      \(A : Type) ->
      \(B : Type) ->
      \(f : A -> B) ->
      \(result : Result A) ->
        Lude.Structures.Result.mapSuccess Error A B f result : Result B

let Result/flatMap =
      \(A : Type) ->
      \(B : Type) ->
      \(f : A -> Result B) ->
      \(result : Result A) ->
        Lude.Structures.Result.flatMap Error A B f result : Result B

let Result/traverseList =
      \(A : Type) ->
      \(B : Type) ->
      \(f : A -> Result B) ->
      \(list : List A) ->
        Lude.Structures.Result.traverseList Error A B f list : Result (List B)

let module =
      \(Input : Type) ->
      \(Output : Type) ->
        let Result = Result Output

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
    , Error
    , Error/nest
    , Error/message
    , Result
    , Result/nest
    , Result/message
    , Result/map
    , Result/flatMap
    , Result/traverseList
    , module
    , Import
    , Import/render
    , Imports
    }
