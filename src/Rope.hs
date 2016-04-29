-- A rope is a tree-like data structure that provides a more efficient
-- way of concatenating strings. A valid rope must be either a node or
-- a leaf. More specifically:
--
--   - A rope leaf always contains some string (whether it be empty or
--     not).
--
--   - A rope node contains no string, but references to it's left and
--     right child nodes. The total length of all it's children's is a
--     rope node's value.

module Rope
     ( Rope(Leaf, Node)
     , concat'
     , delete
     , index
     , insert
     , length'
     , substring
     , toString
     ) where

data Rope = Leaf String
          | Node Int Rope Rope
          deriving(Show, Eq)

-- Concatenates two Ropes.
-- Time complexity: O(1)
concat' :: Rope -> Rope -> Rope
concat' r1 r2 = Node weight r1 r2
  where weight = (length' r1) + (length' r2)

-- Deletes substring in Rope for given range.
-- Time complexity: O(log N)
delete :: Rope -> Int -> Int -> Rope
delete (Leaf string) i j = Node weight (Leaf s1) (Leaf s2)
  where (s1, tmp) = splitAt i string
        (_, s2)   = splitAt (j - i) tmp
        weight    = length (s1 ++ s2)
delete (Node weight left right) i j
  | i > j                 = error "Cannot delete a negative range"
  | i < llen && j < llen  = Node w (delete left i j) right
  | i < llen && j >= llen = Node w (delete left i llen) (delete right 0 (j - llen))
  | i >= llen             = Node w left (delete right (i - llen) (j - llen))
  where llen = length' left
        w    = weight - (j - i)

-- Get Char from Rope at index.
-- Time complexity: O(log N)
index :: Rope -> Int -> Char
index (Leaf string) n = string !! n
index (Node weight left right) n
  | n < (length' left) = index left n
  | otherwise          = index right (n - (length' left))

-- Inserts String into Rope at index.
-- Time complexity: O(log N)
insert :: Rope -> Int -> String -> Rope
insert (Leaf oldString) n newString = Node w1 (Node w2 (Leaf s1) (Leaf s2)) (Leaf s3)
  where (s1, s3) = splitAt n oldString
        s2       = newString
        w1       = length (s1 ++ s2 ++ s3)
        w2       = length (s1 ++ s2)
insert (Node weight left right) n string
  | n < (length' left) = Node w1 (insert left n string) right
  | n > (length' left) = Node w1 left (insert right (n - (length' left)) string)
  | otherwise          = Node w1 left (Node w2 (Leaf string) right)
  where w1 = weight + (length string)
        w2 = (length' right) + (length string)

-- Gets length of Rope.
-- Time complexity: O(1)
length' :: Rope -> Int
length' (Leaf string)     = length string
length' (Node weight _ _) = weight

-- Gets substring for range in Rope.
-- Time complexity: O(log N)
substring :: Rope -> Int -> Int -> String
substring (Leaf string) i j = s
  where (_, tmp) = splitAt i string
        (s, _)   = splitAt (j - i) tmp
substring (Node _ left right) i j
  | i > j                 = error "Cannot substring a negative range"
  | i < llen && j < llen  = substring left i j
  | i < llen && j >= llen = (substring left i llen) ++ (substring right 0 (j - llen))
  | i >= llen             = substring right (i - llen) (j - llen)
  where llen = length' left

-- Casts Rope into String.
-- Time complexity: O(N)
toString :: Rope -> String
toString (Leaf string)       = string
toString (Node _ left right) = (toString left) ++ (toString right)
