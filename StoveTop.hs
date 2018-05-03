import Data.Set (Set)
import Queue (Queue)

import qualified Data.Set as Set
import qualified Queue as Q

data IdName = IdName {iden :: Integer, name :: String}
instance Eq IdName where
  a == b = id a == id b

instance Ord IdName where
  a <= b = id a <= id b

data Term = BoundVar IdName | FreeVar IdName | Function IdName [Term]

data Formula = Predicate IdName [Term]
             | Not Formula
             | And (Set Formula)
             | Forall IdName Formula

data TruthTree = TruthTree {
  closed   :: Bool,
  atomics  :: Set Formula,
  formulae :: Set Formula,
  freeVars :: Set IdName,
  foralls  :: Queue (Formula,(Set IdName))
}

treeDone :: TruthTree -> Bool
treeDone tt = closed tt || Set.null (formulae tt) && Q.all ((freeVars tt==) . snd) (foralls tt)
