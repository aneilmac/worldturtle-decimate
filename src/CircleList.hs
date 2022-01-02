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

-- Remove the nth item. Returns all computations.
removeNthScan :: Int -> CircleList a -> [CircleList a]
removeNthScan 1 cl = [cl, remove cl]
removeNthScan n cl = cl : removeNthScan (n - 1) (next cl)

-- Iterate a function until the predicate is true, returning all computations.
iterateUntilScan :: (CircleList a -> [CircleList a])
                 -> (CircleList a -> Bool)
                 -> CircleList a 
                 -> [CircleList a]
iterateUntilScan f p x = 
    if p x then [x] else let xs = f x 
                          in init xs ++ iterateUntilScan f p (last xs)

-- Let Roman Centurion Carnage Maximus loose. If we have prisoners
-- 1, 2, 3, 4, 5 and nth killed prisoner is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5. 
-- This continues until there is only one prisoner remaining.
romanHistory :: Int -- ^  Number of prisoners (must be 1 or greater).
             -> Int -- ^ Nth prisoner to kill.
             -> [CircleList Int] -- ^ CircleList containing the history of kills.
romanHistory numPrisoners nthKilled = 
    let prisoners = mkCircleList (1, [2..numPrisoners])
     in iterateUntilScan (removeNthScan nthKilled) isSingleton prisoners

-- The first argument is the number of prisoners (must be 1 or greater).
-- The second argument which nth prisoner is killed, i.e. if we have prisoners
-- 1, 2, 3, 4, 5 and this argument is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5.
-- The result is the single remaining prisoner.
chosenPrisoner :: Int -> Int -> Int
chosenPrisoner numPrisoners = current . last . romanHistory numPrisoners
