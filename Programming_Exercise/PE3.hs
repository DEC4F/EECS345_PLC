{- takes a list and removes duplicate elements. -}
removedups :: [Int] -> [Int]
removedups [a] = [a]
removedups lst =
  if (head lst) == (head . tail) lst
    then
      removedups (tail lst)
    else
      (head lst) : removedups (tail lst)

{- Create a continuation passing version, removedups-cps -}
removedups_cps :: [Int] -> ([Int] -> p) -> p
removedups_cps [a] return = return [a]
removedups_cps lst return =
  if (head lst) == (head . tail) lst
    then
      removedups_cps (tail lst) return
    else
      removedups_cps (tail lst) (\v -> return ((head lst) : v))

{- Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists -}
data StarList s = Element Int | Sublist s deriving (Show)

{- Create the function gremovedups that takes a list containing elements and sublists and returns a list with the same structure, but if any "element" is preceded by an identical element, that element is removed 
gremovedups :: [StarList] -> [StarList]
gremovedups [a] = [a]
gremovedups lst =
  if :t (head lst) == Sublist -- is a sublist/not element
    then
      gremovedups (head lst) : gremovedups (tail lst)
    else if (head lst) == (head . tail) lst
      then
        gremovedups (tail lst)
      else
        (head lst) : gremovedups (tail lst) -}
