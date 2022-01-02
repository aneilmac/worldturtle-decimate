module CircleList where

import Data.Functor.Identity (runIdentity)

-- A non-empty ring of elements.
--
-- `CL left x right` represents a circular list whose focus is `x`, followed by 
-- `right`, followed by `left` but in _reverse_.
--
-- E.g.: `CL [2,1] 3 [4,5]` is
--
-- [3,4,5,1,2]
--  |       ^
--  \______/
--
-- where the element after 2 is 3 and the elment before 3 is 2.
data CircleList a = CL [a] a [a]

-- Show instance for a CirclList with the focused element at the front.
instance Show a => Show (CircleList a) where
    show (CL left x right) = show ([x] ++ right ++ (reverse left))

-- Make a circular list with x as the focus and the other elements in order
-- after it.
mkCircleList :: (a, [a]) -> CircleList a
mkCircleList (x, xs) = CL [] x xs

-- Get the current focus of the circular list.
current :: CircleList a -> a
current (CL _ x _) = x

-- Check if the circular list consists of only one element.
isSingleton :: CircleList a -> Bool
isSingleton (CL [] _ []) = True
isSingleton _            = False

-- Change the focus to the next element of the circular list.
--
-- Note that when there are no elemnts to the right we must reverse the elements
-- from the left and take the head of that.
next :: CircleList a -> CircleList a
next (CL [] x []) = CL [] x []
next (CL left x []) = let (y:ys) = reverse left in CL [x] y ys
next (CL left x (y:right)) = CL (x:left) y right

-- Remove the focused element from the list and put the next element in focus.
--
-- Note again we have a special case where there are no elements to the right.
remove :: CircleList a -> CircleList a
remove (CL [] x []) = CL [] x []
remove (CL left x []) = let (y:ys) = reverse left in CL [] y ys
remove (CL left x (y:right)) = CL left y right

-- Remove the nth item.
removeNthM :: Monad m 
           => (CircleList a -> m (CircleList a)) 
           -> Int 
           -> CircleList a 
           -> m (CircleList a)
removeNthM f 1 cl = f cl >>= return . remove
removeNthM f n cl = f cl >>= removeNthM f (n - 1) . next

-- Iterate a function until the predicate is true, has a side effect monad `f`
-- applied to each step.
iterateUntilM :: Monad m
              => (CircleList a -> m (CircleList a))
              -> (CircleList a -> Bool) 
              -> CircleList a 
              -> m (CircleList a)
iterateUntilM f p x = if p x then return x else f x >>= iterateUntilM f p

-- Let Roman Centurion Carnage Maximus loose. If we have prisoners
-- 1, 2, 3, 4, 5 and nth killed prisoner is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5. 
-- This continues until there is only one prisoner remaining.
romanHistoryM :: Monad m 
              => (CircleList Int -> m (CircleList Int)) -- ^ Side effect applied to each step.
              -> Int -- ^  Number of prisoners (must be 1 or greater).
              -> Int -- ^ Nth prisoner to kill.
              -> m (CircleList Int) -- ^ CircleList containing the single remaining prisoner.
romanHistoryM f numPrisoners nthKilled = 
    let prisoners = mkCircleList (1, [2..numPrisoners])
     in iterateUntilM (removeNthM f nthKilled) isSingleton prisoners >>= f

-- Variant of romanHistoryM which has no side effects and is effectively a pure 
-- function.
romanHistory :: Int -> Int -> CircleList Int
romanHistory p k = runIdentity $ romanHistoryM return p k

-- The first argument is the number of prisoners (must be 1 or greater).
-- The second argument which nth prisoner is killed, i.e. if we have prisoners
-- 1, 2, 3, 4, 5 and this argument is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5.
-- The result is the single remaining prisoner.
chosenPrisoner :: Int -> Int -> Int
chosenPrisoner numPrisoners = current . romanHistory numPrisoners