let Deps = ./Deps/package.dhall

let Sdk = Deps.Sdk

in  Sdk.module { major = 1, minor = 0 } ./Config.dhall ./compile.dhall
