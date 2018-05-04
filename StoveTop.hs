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

treeDone :: TruthTree -> Bool
treeDone tt = closed tt || Set.null (formulae tt) && Q.all ((freeVars tt==) . snd) (foralls tt)
