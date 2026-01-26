let MacaddrLibrary = < Ip | Iproute >

let Sig
    : Type
    = { hasqlVersion : Text, macaddrLibrary : MacaddrLibrary }

let default
    : Sig
    = { hasqlVersion = "1.10.1", macaddrLibrary = MacaddrLibrary.Ip }

in  { Type = Sig, default, MacaddrLibrary }
