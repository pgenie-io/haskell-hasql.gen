let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Prelude = Prelude // { Text = Prelude.Text // Lude.Extensions.Text }

let Input =
      { packageName : Text
      , version : Text
      , publicModules : List Text
      , privateModules : List Text
      }

let Output = { path : Text, content : Text }

let compile
    : Input -> Output
    = \(input : Input) ->
        let publicModules = Prelude.Text.concatSep "\n" input.publicModules

        let privateModules = Prelude.Text.concatSep "\n" input.privateModules

        let path = "${input.packageName}.cabal"

        let content =
              ''
              name: ${input.packageName}
              version: ${input.version}

              library
                default-language: Haskell2010
                default-extensions: ApplicativeDo, Arrows, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingStrategies, DerivingVia, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, ImportQualifiedPost, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoFieldSelectors, NoImplicitPrelude, NoMonomorphismRestriction, NumericUnderscores, OverloadedRecordDot, OverloadedStrings, ParallelListComp, PatternGuards, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, StrictData, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, UnboxedTuples, ViewPatterns 
                hs-source-dirs: library
                exposed-modules:
                  ${Prelude.Text.indent 4 publicModules}

                other-modules:
                  ${Prelude.Text.indent 4 privateModules}

                build-depends:
                  aeson,
                  base,
                  bytestring,
                  containers,
                  hasql,
                  text,
                  time,
                  uuid,
                  vector,
              ''

        in  { path, content }

in  { Input, Output, compile }
