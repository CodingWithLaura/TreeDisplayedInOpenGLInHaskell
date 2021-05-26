module TestTrees where
import LeafTree

testbaum_easy :: Tree Int
testbaum_easy = Branch 11 (Leaf 21) (Leaf 22)

testbaum0 :: Tree Int
testbaum0 = Branch 11 (Leaf 21) (Leaf 22)


testbaum1 :: Tree Int
testbaum1 = Branch 11 (Branch 21 (Leaf 31) (Leaf 32)) (Leaf 22)


testbaum2 :: Tree Int
testbaum2 = Branch 11 (Branch 21 (Branch 31 (Leaf 41) (Leaf 42)) (Leaf 32)) (Leaf 22)

testbaum_doof = Branch 11 (Branch 21 (Branch 31 (Leaf 41) (Leaf 42)) (Branch 32 (Leaf 43) (Leaf 44))) (Leaf 22)
