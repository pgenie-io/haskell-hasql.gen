-- TODO: provide an encapsulation of both Algebra and Modules.
-- Thus wrapping a system under a simpler interface.
let Algebra = ./Algebra.dhall

let Modules = ./Modules/package.dhall

in  { Algebra, Modules }
