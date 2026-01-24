let Prelude = ../../Prelude.dhall

let CodegenKit = ../../CodegenKit.dhall

let Name = CodegenKit.Name

let Self = ./Type.dhall

in  Prelude.NonEmpty.singleton Name.Type
