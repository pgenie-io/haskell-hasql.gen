let Algebra = ./Algebra/package.dhall

let Deps = ../Deps/package.dhall

let Params =
      { packageName : Text
      , rootNamespace : Text
      , customTypeNames : List Text
      , statementModuleNames : List Text
      , version : Text
      }

in  Algebra.module
      Params
      ( \(params : Params) ->
          ''
          cabal-version: 3.4
          name: ${params.packageName}
          version: ${params.version}

          library
            hs-source-dirs: src

            default-language: Haskell2010

            default-extensions:
              ApplicativeDo, Arrows, BangPatterns, BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingStrategies, DerivingVia, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, ImportQualifiedPost, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoFieldSelectors, NoImplicitPrelude, NoMonomorphismRestriction, NumericUnderscores, OverloadedRecordDot, OverloadedStrings, ParallelListComp, PatternGuards, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, StrictData, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, UnboxedTuples, ViewPatterns

            exposed-modules:
              ${params.rootNamespace}
              ${params.rootNamespace}.CustomTypes
              ${params.rootNamespace}.Statements
              
            other-modules:
              ${params.rootNamespace}.Prelude
              ${Deps.Prelude.Text.concatMapSep
                  ("\n" ++ "    ")
                  Text
                  ( \(name : Text) ->
                      params.rootNamespace ++ ".CustomTypes." ++ name
                  )
                  params.customTypeNames}
              ${Deps.Prelude.Text.concatMapSep
                  ("\n" ++ "    ")
                  Text
                  ( \(name : Text) ->
                      params.rootNamespace ++ ".Statements." ++ name
                  )
                  params.statementModuleNames}

            build-depends:
              aeson >=2 && <3,
              base >=4.14 && <5,
              bytestring >=0.10 && <0.13,
              containers >=0.6 && <0.9,
              hasql ^>=1.10.1,
              hasql-mapping,
              scientific >=0.3 && <0.4,
              text >=1.2 && <3,
              time >=1.9 && <2,
              uuid >=1.2 && <2,
              vector >=0.12 && <0.14,
          ''
      )
