let Algebra = ../Algebras/Template/package.dhall

let Prelude = ../Prelude.dhall

let Lude = ../Lude.dhall

let Member = { fieldName : Text, sig : Text }

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
              ${params.rootNamespace}.Preludes.CustomType
              ${params.rootNamespace}.Preludes.Statement
              ${Prelude.Text.concatMapSep
                  ("\n" ++ "    ")
                  Text
                  ( \(name : Text) ->
                      params.rootNamespace ++ ".CustomTypes." ++ name
                  )
                  params.customTypeNames}
              ${Prelude.Text.concatMapSep
                  ("\n" ++ "    ")
                  Text
                  ( \(name : Text) ->
                      params.rootNamespace ++ ".Statements." ++ name
                  )
                  params.statementModuleNames}

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
      )
