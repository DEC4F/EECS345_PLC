{- 1. takes a list and removes duplicate elements. -}
removedups :: [Int] -> [Int]
removedups [a] = [a]
removedups lst
  | (head lst) == (head . tail) lst = removedups (tail lst)
  | otherwise                       = (head lst) : removedups (tail lst)

{- 2. Create a continuation passing version, removedups-cps -}
removedups_cps :: [Int] -> ([Int] -> p) -> p
removedups_cps [a] return = return [a]
removedups_cps lst return
  | (head lst) == (head . tail) lst = removedups_cps (tail lst) return
  | otherwise                       = removedups_cps (tail lst) (\v -> return ((head lst) : v))

{- 3. Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists -}
-- data StarList s = Nothing | Element Int | Sublist s [StarList s] deriving (Show)
data StarList s = Element Int | Sublist s deriving (Show)
gettype (Element a) = "Element"
gettype (Sublist a) = "Sublist"

{- 4. Create the function gremovedups that takes a list containing elements and sublists and returns a list with the same structure, but if any "element" is preceded by an identical element, that element is removed -}
gremovedups :: [StarList] -> [StarList]
gremovedups [a] = [a]
gremovedups lst
  | (gettype . head) lst /= "Element" = gremovedups (head lst) : gremovedups (tail lst)
  | (head lst) == (haed . tail) lst   = gremovedups (tail lst)
  | otherwise                         = (head lst) : gremovedups (tail lst)

{- 5.  -}
