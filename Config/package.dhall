let MacaddrLibrary = < Ip >

let Config =
      { hasqlVersionOverride : Optional Text
      , macaddrLibrary : Optional MacaddrLibrary
      }

in  { Config, MacaddrLibrary }
