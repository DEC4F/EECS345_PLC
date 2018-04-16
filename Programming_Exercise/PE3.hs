{- 1. takes a list and removes duplicate elements. -}
removedups :: [Int] -> [Int]
removedups [a] = [a]
removedups lst
  | head lst == (head . tail) lst = removedups (tail lst)
  | otherwise                     = (head lst) : removedups (tail lst)

{- 2. Create a continuation passing version, removedups-cps -}
removedups_cps :: [Int] -> ([Int] -> p) -> p
removedups_cps [a] return = return [a]
removedups_cps lst return
  | head lst == (head . tail) lst = removedups_cps (tail lst) return
  | otherwise                     = removedups_cps (tail lst) (\v -> return ((head lst) : v))

{- 3. Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists -}
data Starlist s = Element Int | Sublist [Starlist s] deriving (Eq, Show)

{- 4. Create the function gremovedups that takes a list containing elements and sublists and returns a list with the same structure, but if any "element" is preceded by an identical element, that element is removed -}
gremovedups :: [Starlist a] -> [Starlist a]
gremovedups [] = []
gremovedups [Element a] = [Element a] 
gremovedups [Sublist a] = [Sublist (gremovedups a)] -- can't reach inside the sublist
gremovedups lst
  | head lst == (head . tail) lst = gremovedups (tail lst)
  | otherwise                     = (head lst) : (gremovedups .tail) lst
 
{- 5. Using the Tree type created in class (use a version without lists), write a function bubbledown that takes a Tree as input. If the element stored in the root is larger than either children, swap the element with the smaller child, and recurse on the child you swapped the element with. The recursion should stop when either you reach a leaf or when the element of the node is smaller than both its children. -}
data Tree t = Leaf t | Internal t (Tree t) (Tree t) deriving (Show)

bubbledown :: (Tree a) -> (Tree a)
bubbledown (Leaf a) = (Leaf a)
bubbledown (Internal a) = (Internal )
