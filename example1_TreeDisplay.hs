import Graphics.UI.GLUT
import LeafTree
import LeafPosTree
import TestTrees
import OpenGLTools

main = do
  getArgsAndInitialize
  meinWindow "Tree in OpenGL"
  mainLoop

meinWindow window_name = do
  createWindow window_name
  displayCallback $= drawTree

drawTree = do
  (drawLeafPosTree displayCircleWrap (mapLeafPosTree (scalePos 0.1) (pos_vom_tree_wrap testbaum_doof)))
  flush





