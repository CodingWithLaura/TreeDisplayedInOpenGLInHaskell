module LeafTree where
data Tree a = Branch a (Tree a) (Tree a) | Leaf a
  

instance Eq a => Eq(Tree a) where
          Leaf x == Leaf y = x == y
          Leaf _ == Branch _ _ _ = False
          Branch _ _ _ == Leaf _ = False
          Branch x xubl xubr == Branch y yubl yubr = x == y && xubl == yubl && xubr == yubr

instance Show a => Show (Tree a) where
         show (Leaf x) = "L" ++ (show x)
         show (Branch x xubl xubr) = "B" ++ (show x)
                                              ++ " (" ++ (show xubl) ++ ") "
                                              ++ "(" ++ (show xubr) ++ ")"
baum_size :: Tree a -> Int
baum_size (Leaf a) = 1
baum_size (Branch x xubl xubr) = 1 + (baum_size xubl) + (baum_size xubr)

mein_baum = Branch 12 (Branch 6 (Leaf 2) (Leaf 3)) (Branch 8 (Leaf 4) (Leaf 7))

tiefensuche :: Tree a -> [a]
tiefensuche (Leaf x) = [x]
tiefensuche (Branch x xubl xubr) = x : ((tiefensuche xubl) ++ (tiefensuche xubr))

breitensuche :: (Eq a) => Tree a -> [a]
breitensuche baum_in = breitensuchliste [baum_in] []


breitensuchliste :: (Eq a) => [Tree a] -> [Tree a] -> [a]
breitensuchliste [] [] = []
--
breitensuchliste [] akku = breitensuchliste akku []
--
breitensuchliste ((Leaf x):xs) akku = x : (breitensuchliste xs akku)
breitensuchliste ((Branch x xubl xubr):xs) akku = x: (breitensuchliste xs (akku ++ [xubl] ++[xubr])) 
