-------------------------------------------------------------
--------------------Programming Task 5-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------
import BinaryTrees
import AltBinaryTrees

-------------------------------------------------------------
-- PROBLEM #1
-- Purpose: 
--    pairTree btr
--       This function takes in a binary tree and returns a similar binary tree
--       in stucture, but each node label is represented as a pair (ie. x -> (x,x))

-- Definition:
pairTree :: BTree a -> BTree (a,a)
pairTree Empty = Empty
pairTree (BNode x left right) = (BNode (x,x) (pairTree left) (pairTree right))

-- Tests:
--   Tests include an empty tree, a tree with the root node, another tree with just the root node but a different
--   value Type, and a tree with multiple nodes.

t1a = pairTree Empty                    -- Should return Empty (Empty tree has no nodes to perform x -> x,x)
t1b = pairTree (BNode 'z' Empty Empty)  -- Should return BNode ('z','z') Empty Empty (Tree with 1 node)
t1c = pairTree tree1                    -- Should return BNode (7,7) Empty Empty (Another tree w/ one node but diff value Type)
t1d = pairTree tree2  -- Should return BNode (8,8) (BNode (13,13) Empty (BNode (12,12) Empty Empty)) (BNode (20,20) Empty Empty) (Multiple Nodes)

-------------------------------------------------------------
-- PROBLEM #2
-- Purpose: 
--    leaves btr
--      Returns a list that contains the labels of all the leaf nodes in the binary tree passed in

-- Definition:
leaves :: BTree a -> [a]
leaves Empty = []
leaves (BNode x Empty Empty) = [x]
leaves (BNode x left right) = leaves left ++ leaves right

-- Tests:
--   Tests include an empty tree, 2 different trees with just one node, and a tree with multiple nodes,
--   and multiple leaves.

t2a = leaves Empty                    -- Should return [] (Empty tree has no leaves)
t2b = leaves (BNode 'z' Empty Empty)  -- Should return ['z'] or "z" (Tree with 1 node)
t2c = leaves tree1                    -- Should return [7] (Another tree w/ one node but diff value Type)
t2d = leaves tree5                    -- Should return [65,78,134,120] (Multiple Nodes, and multiple leaves)

-------------------------------------------------------------
-- PROBLEM #3
-- Purpose: 
--    full btr
 --       This function takes in a binary tree and returns a boolean indicating if the binary tree is a full tree.
--       Will return true if it is a full tree, else false.

-- Definition:
full :: BTree a -> Bool
full Empty = True
full (BNode _ Empty Empty) = True
full (BNode _ Empty right) = False
full (BNode _ left Empty) = False
full (BNode _ left right) = (full left) && (full right)

-- Tests:
--   Tests include an empty tree, a tree with 1 node, 2 full trees, and 2 trees that are not full.

t3a = full Empty                                                   -- Should return True (Empty tree)
t3b = full (BNode 3 Empty Empty)                                   -- Should return True (Only 1 node)
t3c = full (BNode 3 (BNode 4 Empty Empty) (BNode 5 Empty Empty))   -- Should return True (Root node with 2 leaf node children)
t3d = full (BNode 3 (BNode 4 Empty Empty) Empty)                   -- Should return False (Root node has only 1 child)
treeT3e = (BNode 3 (BNode 4 Empty Empty) (BNode 5 (BNode 6 Empty Empty) (BNode 7 Empty Empty)))
t3e = full treeT3e                                                 -- Should return True (All nodes have 2 children or are leaves)
treeT3f = (BNode 3 (BNode 4 Empty Empty) (BNode 5 (BNode 6 Empty Empty) Empty))
t3f = full treeT3f                                                 -- Should return False (Has a node with 1 child node)

-------------------------------------------------------------
-- PROBLEM #4
-- Purpose: 
--    rightmost backup btr
--       This function takes in a binary tree and returns a boolean indicating if the binary tree is a full tree.
--       Will return true if it is a full tree, else false.

-- Definition:
rightmost :: a -> BTree a -> a
rightmost backup (BNode x _ Empty) = x
rightmost backup (BNode x _ right) = (rightmost backup right)
rightmost backup Empty = backup

-- Tests:
--   Tests include an empty tree, a tree with one node, a 2 full trees, a tree in which the root node was the rightmost,
--      despite it not being the deepest in the tree, and another example of a node being the rightmost but not the deepets.

t4a = rightmost 99 Empty                                                   -- Should return 99 (Empty tree, backup returned)
t4b = rightmost 99 (BNode 3 Empty Empty)                                   -- Should return 3 (Only 1 node)
t4c = rightmost 99 (BNode 3 (BNode 4 Empty Empty) (BNode 5 Empty Empty))   -- Should return 5 (Root node with 2 leaf node children)
t4d = rightmost 99 (BNode 3 (BNode 4 (BNode 5 Empty Empty) Empty) Empty)   -- Should return 3 (Root node has no right child)
treeT4e = (BNode 3 (BNode 4 Empty Empty) (BNode 5 (BNode 6 Empty Empty) (BNode 7 Empty Empty)))
t4e = rightmost 99 treeT3e                                                 -- Should return 7 (Another full tree)
treeT4f = (BNode 3 (BNode 4 Empty Empty) (BNode 5 (BNode 6 Empty Empty) Empty))
t4f = rightmost 99 treeT3f                                                 -- Should return 5 (Another case where the rightmost is not the deepest)

-------------------------------------------------------------
-- PROBLEM #5
-- Purpose: 
--    pairTreeA atr
--       This function takes an alternate binary tree and returns a similar binary tree
--       in stucture, but each node label is represented as a pair (ie. x -> (x,x))

-- Definition:
pairTreeA :: AltTree a -> AltTree (a,a)
pairTreeA (Leaf a) = Leaf (a,a)
pairTreeA (One a (node)) = One (a,a) (pairTreeA node)
pairTreeA (Two a lft rght) = Two (a,a) (pairTreeA lft) (pairTreeA rght) 

-- Tests:
--   Tests include a couple trees with just the root node, and two other trees with multiple nodes.  Also there are several different data types
--      associated with the values at each node (ie. chars, integers, strings)

t5a = pairTreeA (Leaf 'z')   -- Should return Leaf ('z','z') (AltTree with 1 node)
t5b = pairTreeA atree1       -- Should return Leaf (7,7)  (Another AltTree w/ one node but diff value Type)
t5c = pairTreeA atree5       -- Should return Two (100,100) (Two (75,75) (Leaf (65,65)) (Two (82,82) (Leaf (78,78)) (Leaf (134,134)))) (Leaf (120,120)) (Multiple Nodes)
t5d = pairTreeA atree4       -- Should return Two ("here's","here's") (One ("a","a") (Leaf ("tree","tree"))) (Leaf ("string","string")) (Multiple Nodes)

-------------------------------------------------------------
-- PROBLEM #6
-- Purpose: 
--    leavesA atr
--      Returns a list that contains the labels of all the leaf nodes in the alternate binary tree passed in

-- Definition:
leavesA :: AltTree a -> [a]
leavesA (Leaf a) = [a]
leavesA (One a (node)) = (leavesA node)
leavesA (Two a lft rght) = (leavesA lft) ++ (leavesA rght)

-- Tests:
--   Tests include 2 different trees with just one node, and a couple trees with multiple nodes,
--   and multiple leaves.

t6a = leavesA (Leaf 'z')    -- Should return ['z'] or "z"  (Tree with 1 node)
t6b = leavesA atree1        -- Should return [7] (Another tree w/ one node but diff value Type)
t6c = leavesA atree5        -- Should return [65,78,134,120] (Multiple Nodes, and multiple leaves)
t6d = leavesA atree2        -- Should return [12,20] (Multiple nodes, multiple leaves)
t6e = leavesA atree4        -- Should return ["tree","string"] (Multiple leaves, different data type strings)

-------------------------------------------------------------
-- PROBLEM #7
-- Purpose: 
--    fullA atr
--       This function takes an alternate binary tree and returns a boolean indicating if binary tree is a full tree
--       Will return true if it is a full tree, else false.

-- Definition:
fullA :: AltTree a -> Bool
fullA (Leaf _) = True
fullA (One _ _) = False
fullA (Two _ lft rght) = (fullA lft) && (fullA rght)

t7a = fullA (Leaf 12)                                  -- Should return True (Only 1 node)
t7b = fullA (Two 3 (Leaf 4) (Leaf 5))                  -- Should return True (Root node with 2 leaf node children)
t7c = fullA (One 'c' (Leaf 'a'))                       -- Should return False (Root node has only 1 child)
atreeT7d = (Two 3 (Two 4 (Leaf 6) (Leaf 8)) (Two 7 (Leaf 1) (Leaf 2)))
t7d = fullA atreeT7d                                   -- Should return True (All nodes have 2 children or are leaves)
atreeT7e = (Two 3 (Two 4 (Leaf 6) (Leaf 8)) (One 4 (Leaf 6)))
t7e = fullA atreeT7e                                   -- Should return False (Has a node with only 1 child node)

-------------------------------------------------------------
-- PROBLEM #8
-- Purpose: 
--    rightmostA atr
--       This function takes an alternate binary tree and returns the value at the right most node
--       of the tree structure.

-- Definition:
rightmostA :: AltTree a -> a
rightmostA (Leaf x) = x
rightmostA (One x node) = rightmostA node
rightmostA (Two x lft rght) = rightmostA rght

-- Tests:
--   Tests include a tree with one node, trees without any Two nodes, trees with Two nodes and an example of a node being the rightmost but not the deepest
--      and a tree with a string of One nodes.

t8a = rightmostA (Leaf 3)                                -- Should return 3 (Only 1 node)
t8b = rightmostA (Two 3 (Leaf 4) (Leaf 5))               -- Should return 5 (Root node with 2 leaf node children)
t8c = rightmostA (One 'a' (Leaf 'z'))                    -- Should return 'z' (Root node has one child)
t8d = rightmostA atree4                                  -- Should return "string" (left subtree larger than right subtree)
t8e = rightmostA atree5                                  -- Should return 120 (Another case where the rightmost is not the deepest)
aTreeT8f = (One 3 (One 4 (One 5 (Two 6 (Two 7 (Leaf 9) (Leaf 10)) (One 11 (Leaf 12))))))
t8f = rightmostA aTreeT8f                                -- Should return 12 (String of One nodes leading to a couple Two nodes)

-------------------------------------------------------------
-- PROBLEM #9
-- Purpose: 
--    convert atr
--       This function takes an alternate binary tree and returns an equivalent traditional binary tree.

-- Definition:
convert :: AltTree a -> BTree a
convert (Leaf x) = BNode x Empty Empty
convert (One x node) = BNode x (convert node) Empty
convert (Two x lft rght) = BNode x (convert lft) (convert rght)

-- Tests:
--   Tests include a tree with one node, trees without any Two nodes, trees with Two nodes and a tree with a string of One nodes.

t9a = convert (Leaf 3)                                -- Should return BNode 3 Empty Empty (Only 1 node)
t9b = convert (Two 3 (Leaf 4) (Leaf 5))               -- Should return BNode 3 (BNode 4 Empty Empty) (BNode 5 Empty Empty) (Root node with 2 leaf node children)
t9c = convert (One 'a' (Leaf 'z'))                    -- Should return BNode 'a' (BNode 'z' Empty Empty) Empty (Root node has one child)
t9d = convert atree4                                  -- Should return BNode "here's" (BNode "a" (BNode "tree" Empty Empty) Empty) (BNode "string" Empty Empty) (left subtree larger than right subtree)

t9e = convert atree5                                  -- Should return BNode 100 (BNode 75 (BNode 65 Empty Empty) (BNode 82 (BNode 78 Empty Empty) (BNode 134 Empty Empty))) (BNode 120 Empty Empty) (Larger unbalanced tree)

aTreeT9f = (One 3 (One 4 (One 5 (Two 6 (Two 7 (Leaf 9) (Leaf 10)) (One 11 (Leaf 12))))))
t9f = convert aTreeT8f                                -- Should return BNode 3 (BNode 4 (BNode 5 (BNode 6 (BNode 7 (BNode 9 Empty Empty) (BNode 10 Empty Empty)) (BNode 11 (BNode 12 Empty Empty) Empty)) Empty) Empty) Empty (String of One nodes leading to a couple Two nodes)
