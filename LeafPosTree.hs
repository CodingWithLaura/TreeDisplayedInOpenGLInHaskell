module LeafPosTree where
import LeafTree

data TreePos a b = LeafPos a b
                   | BranchPos a b (TreePos a b ) (TreePos a b)
                   deriving (Eq, Show)  

mapLeafPosTree :: (b -> c ) -> TreePos a b -> TreePos a c
mapLeafPosTree f (LeafPos wert pos) = LeafPos wert (f pos)
mapLeafPosTree f (BranchPos wert pos xubl xubr) = (BranchPos wert (f pos) (mapLeafPosTree f xubl) (mapLeafPosTree f xubr))  

drawLeafPosTree :: (b -> IO()) -> TreePos a b -> IO()
drawLeafPosTree f (LeafPos wert pos) = do
                                         f pos
drawLeafPosTree f (BranchPos wert pos xubl xubr) = do
                                                     f pos
                                                     drawLeafPosTree f xubl
                                                     drawLeafPosTree f xubr
pos_vom_tree_wrap :: Tree a -> TreePos a (Int,Int)
pos_vom_tree_wrap baum = pos_vom_tree baum 0 (-1)

pos_vom_tree :: Tree a -> Int -> Int -> TreePos a (Int, Int) 
pos_vom_tree (Leaf z) zahl tiefe = LeafPos z (zahl, tiefe)
pos_vom_tree (Branch z zubl zubr) zahl tiefe  = BranchPos z (x,y) (pos_vom_tree zubl zahl (tiefe-1)) (pos_vom_tree zubr (x+1) (tiefe-1) ) 
                                    where
                                       x = zahl + (widthAdv_leftside (Branch z zubl zubr)) 
                                       y = tiefe   
                                                     
widthAdv_leftside :: Tree a -> Int
widthAdv_leftside (Leaf a) = 0
widthAdv_leftside (Branch x xubl _) = 1 + (widthAdv_leftside xubl) + (widthAdv_rightside xubl)

widthAdv_rightside :: Tree a -> Int
widthAdv_rightside (Leaf a) = 0
widthAdv_rightside (Branch x _ xubr) = 1 + (widthAdv_leftside xubr) + (widthAdv_rightside xubr)

