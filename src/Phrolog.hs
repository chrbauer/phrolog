{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes , InstanceSigs, FlexibleContexts, UndecidableInstances, FlexibleInstances, RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}

module Phrolog where

import qualified Data.Map as M
import  Data.Map (Map)
import Prelude hiding (Functor)
import Data.List (intercalate)
import Debug.Trace(trace)

class PrologTypes p where
  data AtomType p
  data VariableType p
  data FunctorType p


type IndexedVar p = ((VariableType p), Int)
data Term p = Atom !(AtomType p) | Var (IndexedVar p) | Compound !(FunctorType p) !Int ![Term p]
data Axiom p = Axiom (Term p) [Term p] 
type Program p = [Axiom p]

type Substitution p = Map (IndexedVar p) (Term p)

class ToTerm a p | a -> p where
  toTerm :: a -> Term p


instance ToTerm (AtomType p) p where
  toTerm = Atom


instance ToTerm (VariableType p) p where
  toTerm v = Var (v, 0)

instance ToTerm (Term p) p  where
  toTerm = id


instance (Show (AtomType p), Show (VariableType p), Show (FunctorType p)) => Show (Term p) where
     show (Atom a) = show a
     show (Var (x, 0)) = "$" ++ show x
     show (Var (x, i)) = "$" ++ show x ++ show i
     show (Compound f _ ts) = show f ++ "(" ++ terms ++ ")"
             where
               terms :: String
               terms = intercalate ", " $ map show ts

instance (Show (AtomType p), Show (VariableType p), Show (FunctorType p)) => Show (Axiom p) where
  show (Axiom head []) = show head ++ "."
  show (Axiom head claues) = show head ++ " |- " ++ (intercalate ", " $ map show claues) ++ "."


instance  ToTerm t p => ToTerm (FunctorType p, t) p  where
  toTerm (f, t) = Compound f 1 [toTerm t]

instance  (ToTerm t1 p, ToTerm t2 p) => ToTerm (FunctorType p, t1, t2) p  where
  toTerm (f, t1, t2) = Compound f 1 [toTerm t1, toTerm t2]  
    
fact :: ToTerm t p => t -> Axiom p
fact t = Axiom (toTerm t) []

infixl 5 |-
(|-) :: (ToTerm t1 p, ToTerm t2 p)  => t1  ->  t2 -> Axiom p 
(|-) head clause = Axiom (toTerm head) [toTerm clause]


infixl 4 .&
(.&) :: (ToTerm t p)  => Axiom p  ->  t -> Axiom p 
(.&) (Axiom head cls) cl = Axiom  head (cls ++ [toTerm cl])


occursIn v (Atom _) = False
occursIn v (Var w) = v == w 
occursIn v (Compound f _ ts) = any (occursIn v) ts

instantiate subst orig@(Var v) =
    case M.lookup v subst of
      Just replacement -> replacement
      Nothing -> orig
instantiate subst orig@(Atom _) = orig
instantiate subst (Compound f n ts) = Compound f n (map (instantiate subst) ts)


unify :: (Eq (AtomType p), Eq (FunctorType p), Ord (VariableType p)) => Term p -> Term p -> Maybe (Substitution p)
unify t1 t2 =  aux M.empty [(t1, t2)]
  where --aux :: (Eq a, Eq f, Ord v) =>  Substitution a v f -> [(Term a v f, Term a v f)] -> Maybe (Substitution a v f)
        aux subst [] = Just subst
        aux subst ((ht1, ht2):ts) =
                 case (ht1, ht2) of
                   (Var a, Var b) ->
                     if a == b then aux subst ts
                     else applySubst b ht1 subst ts
                   (Atom a, Atom b) ->
                     if a == b then aux subst ts
                     else Nothing
                   (Var x, ht2) ->
                     if  x  `occursIn` ht2 then
                       Nothing
                     else
                       applySubst x  ht2 subst ts
                   (ht1, Var y) ->
                     if occursIn y ht1 then
                       Nothing
                     else
                       applySubst y ht1 subst ts
                   (Compound f1 n1 ts1, Compound f2 n2 ts2) ->
                         if f1 /= f2 || n1 /= n2 then Nothing
                          else aux subst (zip ts1 ts2 ++ ts)
        --applySubst :: (Eq a, Eq f, Ord v) => v -> Term a v f -> Substitution a v f -> [(Term a v f, Term a v f)] -> Maybe (Substitution a v f)
        applySubst v t subst ts = aux subst' ts'
          where --replaceV :: (Eq a, Eq f, Ord v) => Term a v f -> Term a v f 
                replaceV = instantiate (M.singleton v t)
                subst'   = M.map replaceV $ M.insert v t subst
                ts'      = map (\ (t1, t2) -> (replaceV t1, replaceV t2)) ts



maxIndex (Atom a) = 0
maxIndex (Var (_, i)) = i
maxIndex (Compound _ _ ts) = maximum $ map maxIndex ts

renameVars freshIndex = aux
   where aux term@(Atom _) = term
         aux (Var (key, _)) = Var (key, freshIndex)
         aux (Compound f n ts) = Compound f n (map aux ts)

                
solve :: (Eq (AtomType p), Eq (FunctorType p), Ord (VariableType p), Show (Term p)) => Program p -> Term p ->  [Term p] 
solve program goal = prove goal [goal] program
  where prove goal' [] _ = [goal']
        prove goal' _ [] = []
        prove goal' resolvent@(res:rest) (Axiom head clauses:axioms) =
          let freshIndex = maxIndex res + 1
              head' = renameVars freshIndex head in
            case unify res head of
              Nothing -> prove goal' resolvent axioms
              Just mgu -> let inst = instantiate mgu
                              goal'' = inst goal'
                          in
                            prove goal'' (map inst clauses ++ rest) program ++
                            prove goal' resolvent axioms
