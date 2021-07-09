{-# LANGUAGE PostfixOperators, TypeFamilies #-}
module Bible where

import Prelude hiding (Functor)
import Phrolog
--import Control.Monad.Writer.Strict  hiding (Functor)

data Bible = Bible

instance PrologTypes Bible where
  data AtomType Bible = Abraham | Isaac |
    Terach | Nachor | Haran |
    Milcah | Yiscah | Sarah | Lot
    deriving (Eq, Show)
  data VariableType Bible = X | Y deriving (Eq, Ord, Show)

  data FunctorType Bible = Father | Mother | Male | Female | Parent | Son | Grandparent
    deriving (Eq, Ord, Show)

bibleFacts :: Program Bible
bibleFacts = [
  fact (Father, Terach, Abraham),

  fact (Father, Terach, Nachor),
  fact (Father, Terach, Haran),
  fact (Father, Abraham, Isaac),
  fact (Father, Haran, Lot),
  fact (Father, Haran, Milcah),
  fact (Father, Haran, Yiscah),

  fact (Mother, Sarah, Isaac),

  (Parent, X, Y) |-  (Mother, Sarah, Y),
  (Son, X, Y) |-  (Male, X) .& (Parent, Y, X)
  ] ++ [ fact (Male, p) |  p <- [Terach, Abraham, Nachor, Haran, Isaac, Lot] ]
    ++ [ fact (Female, p) | p <- [Sarah, Milcah, Yiscah]]

familyRules :: Program Bible
familyRules =     [
               (Parent, X, Y) |- (Father, X, Y),
               (Parent, X, Y) |- (Mother, Sarah, Y)
             ]

main = do
  putStrLn "Is Abraham the father of Isaac?"
  putStrLn $ if provable bibleFacts (Father, Abraham, Isaac) then "yes" else "don't know"
