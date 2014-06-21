{-
Tree datatype, a Tree is either a Node with data
or empty (Nil)
-}
data Tree a = Node a (Tree a) (Tree a)
            | Nil
              deriving(Show,Eq)
{-
Add a node to Tree, If element already present in the Tree,
then it is ignored.
-}
add :: (Ord a) => Tree a -> a -> Tree a
add Nil elem = Node elem Nil Nil
add (Node val left right) elem
  | elem < val = Node val (add left elem) right
  | elem > val = Node val left (add right elem)
  | otherwise = Node val left right

{- Search a tree -}
search :: (Ord a) => Tree a -> a -> Bool
search Nil _ = False
search (Node elem left right) a
  | elem == a = True
  | elem > a  = search left a
  | otherwise = search right a


{- Tree traversal -}
preOrder :: Tree a -> [a]
preOrder Nil = []
preOrder (Node a left right) =
  a : preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Nil = []
postOrder (Node a left right) =
  postOrder left ++ postOrder right ++ [a]

inOrder :: Tree a -> [a]
inOrder Nil = []
inOrder (Node a left right) =
  inOrder left ++ [a] ++ inOrder right
  
maxElem :: Tree a -> Maybe a
maxElem Nil = Nothing
maxElem (Node elm _ Nil) = Just elm
maxElem (Node a _ left) = maxElem left


minElem :: Tree a -> Maybe a
minElem Nil = Nothing
minElem (Node a Nil _) = Just a
minElem (Node _ right _) = minElem right

delete :: (Ord a) => Tree a -> a -> Tree a
delete (Node a Nil Nil) key
  | a == key = Nil
  | otherwise = Node a Nil Nil
delete (Node a Nil right) key
  | a == key  = right
  | otherwise = Node a Nil (delete right key)
delete (Node a left Nil) key
  | a == key = left
  | otherwise = Node a (delete left key) Nil
delete (Node a left right) key
  | a == key = Node maxLeft (delete left maxLeft) right
  | a > key  = Node a (delete left key) right
  | otherwise = Node a left (delete right key)
  where
    maxLeft = maxElemNonNil left
    maxElemNonNil (Node a _ Nil) = a
    maxElemNonNil (Node a _ left) = maxElemNonNil left
    
{-
Geerate Tree from list, elements are added into tree from
left to right
-}
list2Tree :: (Ord a) => [a] -> Tree a
list2Tree = foldl add Nil

a = [10, 2, 13, 5, 18, 28, 27, 95, 73, 75, 49, 93]

