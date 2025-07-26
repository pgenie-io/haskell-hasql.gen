
let Sdk = ./Sdk.dhall
let Fixtures = Sdk.Fixtures
let fixture1 = Fixtures._1
let Package = ./Package/package.dhall
in 

Package.compile
fixture1


