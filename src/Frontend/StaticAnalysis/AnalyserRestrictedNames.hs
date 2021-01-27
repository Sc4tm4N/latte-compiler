module Frontend.StaticAnalysis.AnalyserRestrictedNames (isRestricted) where

import Prelude hiding (id)

import Frontend.Grammar.AbsLatte


isRestricted :: Ident -> Bool
isRestricted id = id `elem` restrictedNames

restrictedNames :: [Ident]
restrictedNames = [(Ident "self")]