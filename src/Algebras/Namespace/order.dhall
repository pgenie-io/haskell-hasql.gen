let Lude = ../../Lude.dhall

let Self = ./Type.dhall

let CodegenKit = ../../CodegenKit.dhall

in  Lude.Extensions.NonEmpty.order CodegenKit.Name.Type CodegenKit.Name.order
