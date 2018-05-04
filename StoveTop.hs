import Data.Set (Set)
import Queue (Queue)

import qualified Data.Set as Set
import qualified Queue as Q

type Id = Integer

data IdName = IdName {iden :: Id, name :: String}
instance Eq IdName where
  a == b = id a == id b

instance Ord IdName where
  a <= b = id a <= id b

data Term = BoundVar IdName | FreeVar IdName | Function IdName [Term] deriving (Eq,Ord)

data Formula = Equal Term Term
             | Predicate IdName [Term]
             | Not Formula
             | And (Set Formula)
             | Forall IdName Formula
             deriving (Eq,Ord)

-- Invariants:
--  1. closed == True iff for some Formula `a`, both `a` and Not `a` top level
--       formulae in the TruthTree
--  2. Predicates and their negations only appear as top level formulae in atomics
--  3. Negations of Equals only appear as top level formulae in atomics
--  4. Positive Equals do not appear as top level formulae in the TruthTree
--  5. Positive Foralls only appear as top level formulae in foralls
--  6. The snd of each element in foralls is a subset of freeVars
--  7. The fst of each element in foralls has been decomposed with each free
--      variable in snd of that elelement
--  8. Double negation does not appear as a top level formula in the TruthTree
data TruthTree = TruthTree {
  closed   :: Bool,
  atomics  :: Set Formula,
  formulae :: Set Formula,
  freeVars :: Set Id,
  foralls  :: Queue (Formula,(Set Id))
}

empty :: TruthTree
empty = TruthTree {
  closed   = False,
  atomics  = Set.empty,
  formulae = Set.empty,
  freeVars = Set.empty,
  foralls  = Q.empty
}

notConsistent :: Formula -> TruthTree -> Bool
notConsistent _ tt | closed tt = True
notConsistent formula tt =
  case formula of
    Equal t1 t2
      | t1 == t2          -> False
      | t1 /= t2          -> not $ isSound tt
    Not (Equal t1 t2)     -> t1 /= t2
    p@(Predicate _ _)     -> Set.member (Not p) $ atomics tt
    Not p@(Predicate _ _) -> Set.member p $ atomics tt
    a@(And _)             -> Set.member (Not a) $ formulae tt
    Not a@(And _)         -> Set.member a $ formulae tt
    f@(Forall _ _)        -> Set.member (Not f) $ formulae tt
    Not f@(Forall _ _)    -> Q.elem f . fmap fst $ foralls tt

isSound :: TruthTree -> Bool
isSound tt
  | any (flip notConsistent tt) (atomics tt)  = False
  | any (flip notConsistent tt) (formulae tt) = False
  | otherwise                                 = True
-- | Q.any (flip notConsistent tt . fst) (foralls tt) = False (redundant)

treeDone :: TruthTree -> Bool
treeDone tt = closed tt || Set.null (formulae tt) && Q.all ((freeVars tt==) . snd) (foralls tt)
