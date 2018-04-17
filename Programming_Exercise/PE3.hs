{- 1. takes a lis-t and removes duplicate elements. -}
removedups :: [Int] -> [Int]
removedups [a] = [a]
removedups lst
  | head lst == (head.tail) lst = removedups (tail lst)
  | otherwise                   = (head lst) : removedups (tail lst)

{- 2. Create a continuation passing version, removedups-cps -}
removedups_cps :: [Int] -> ([Int] -> p) -> p
removedups_cps [a] return = return [a]
removedups_cps lst return
  | head lst == (head.tail) lst = removedups_cps (tail lst) return
  | otherwise                   = removedups_cps (tail lst) (\v -> return ((head lst) : v))

{- 3. Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists -}
data Starlist s = Element s | Sublist [Starlist s] deriving (Show)
instance (Eq s) => Eq (Starlist s) where
  (Element x) == (Element y) = x == y
  (Element x) == (Sublist y) = False
  (Sublist x) == (Element y) = False
  (Sublist x) == (Sublist y) = False

{- 4. Create the function gremovedups that takes a list containing elements and sublists and returns a list with the same structure, but if any "element" is preceded by an identical element, that element is removed -}
gremovedups [] = []
gremovedups [Element a] = [Element a]
gremovedups [Sublist a] = [Sublist (gremovedups a)]
gremovedups lst
  | head lst == head (tail lst) = gremovedups (tail lst)
  | otherwise                   = gremovedups ([(head lst)]) ++ (gremovedups (tail lst))
 
{- 5. Using the Tree type created in class (use a version without lists), write a function bubbledown that takes a Tree as input. If the element stored in the root is larger than either children, swap the element with the smaller child, and recurse on the child you swapped the element with. The recursion should stop when either you reach a leaf or when the element of the node is smaller than both its children. -}
data Tree t = Leaf t | Internal t (Tree t) (Tree t) deriving (Show)
bubbledown :: (Tree a) -> (Tree a)
bubbledown (Leaf a) = Leaf a
bubbledown (Internal a l r)
  | a > l && r > l = (Internal l (bubbledown a) (bubbledown r)) -- l is smallest
  | a > r && l > r = (Internal r (bubbledown l) (bubbledown a)) -- r is smallest
  | a > l && a < r = (Internal l (bubbledown a) (bubbledown r)) -- l < a < r
  | a > r && a < l = (Internal r (bubbledown l) (bubbledown a)) -- r < a < l
  | otherwise      = (Internal a (bubbledown l) (bubbledown r)) -- a is smallest

{- 6. Using the Maybe monad of Haskell, create a function called checkcons that has the following type. The function takes a Maybe value of some type, a Maybe list of the same type (as a monad), and a test function and returns a Maybe list of the same type. If either Maybe is Nothing, the result is Nothing. If the first Maybe value passes the test function, the result has the first element cons'd onto the front of the list. Otherwise the result is Nothing. -}
checkcons :: Maybe a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
checkcons Nothing _ _ = Nothing
checkcons mx mlist test = do
  x   <- mx
  lst <- mlist
  if (test x)
    then return (x : lst)
    else Nothing

{- 7. Using checkcons create a function checklist that takes a list and a function and returns Nothing if the elements in the list fail to past the function and the list (embedded in a Maybe) if all the elements pass. -}
checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist [] _     = Just []
checklist lst test = checkcons (Just (head lst)) (checklist (tail lst) test) test

{- 8. Create a list monad that generalizes a list. This will not be a Haskell Monad type, but instead one of our own creation like the Value type from lecture. -}
data List a = Null | Pair a (List a) deriving (Eq, Show)

{- Then create a binding function lbind and a return function lreturn to make a list monad. -}
lreturn a = Pair a Null

lbind :: List a -> (a -> List a1) -> List a1
lbind Null _          = Null
lbind (Pair a Null) f = (f a)
lbind (Pair a l) f    = lcons (f a) (lbind l f)

-- helper method that get rid of the extra Null
lcons (Pair a b) (Pair c d) = Pair a (Pair c d)
