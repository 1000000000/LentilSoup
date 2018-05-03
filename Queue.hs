module Queue (Queue(), empty, null, (<:), head, tail, fromList, toList, length, any, all) where

import Prelude hiding (null,length,head,tail,any,all)
import Data.List (genericLength)

import qualified Prelude as P

infixl 5 <:

data Queue a = Queue [a] [a]

instance Eq a => Eq (Queue a) where
  q1 == q2
    | null q1 && null q2       = True
    | null q1 && not (null q2) = False
    | null q2 && not (null q1) = False
    | otherwise                = head q1 == head q2 && tail q1 == tail q2

instance Ord a => Ord (Queue a) where
  q1 <= q2
    | null q1                  = True
    | null q2                  = False
    | otherwise                = head q1 < head q2 || (head q1 == head q2 && tail q1 <= tail q2)

instance Functor Queue where
  fmap fxn (Queue f r) = Queue (fmap fxn f) (fmap fxn r)

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

(<:) :: Queue a -> a -> Queue a
(Queue [] []) <: x = Queue [] [x]
(Queue f  r ) <: x = Queue f (x:r)

head :: Queue a -> a
head (Queue (x:_) _) = x

tail :: Queue a -> Queue a
tail (Queue (_:[]) r) = Queue (reverse r) []
tail (Queue (_:f) r)  = Queue f r

fromList :: [a] -> Queue a
fromList = flip Queue []

toList :: Queue a -> [a]
toList (Queue f r) = f ++ reverse r

length :: Num a => Queue a -> a
length (Queue f r) = genericLength f + genericLength r

any :: (a -> Bool) -> Queue a -> Bool
any p (Queue f r) = P.any p f || P.any p r

all :: (a -> Bool) -> Queue a -> Bool
all p (Queue f r) = P.all p f || P.all p r
