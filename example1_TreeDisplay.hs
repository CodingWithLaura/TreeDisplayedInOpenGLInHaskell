--            B12               y
--           /    \             |
--        B13      B14          |
--        / \       / \         |
--       L1   B3  L4   L5       +----------> x
--              \
--               L3
-- liste Branchen -> position   ->male braune Kreise (zahlen müssen noch rein)
-- liste Leafes   -> positionen  ->malen grüne kreise mit Zahlen drinne
-- verbindungslinien -> positionen(Anfang Ende) ->huihuihui

-- runter gehen (Ebene zu Ebene immer -1)

-- wurzel is 0,0
-- > b12 (0.0, 0.0)
--   b13 ((-2.0),(-1.0))  B14(2.0,(-1.0))
--  L1((-3.0),(-2.0)) L3((-1.0),(-2.0)) L4(1.0,(-2.0)) L5(3.0,(-2.0))
import Graphics.UI.GLUT

branchesPoses :: [(GLfloat,GLfloat,GLfloat)]
branchesPoses = [(0.0,0.0,0.0),
                 ((-2.0),(-1.0),0.0),
                  (2.0,(-1.0),0.0),
                  (2.0,(-1.0),0.0),
                  ((-3.0),(-2.0),0.0),
                  ((-1.0),(-2.0),0.0),
                  (1.0,(-2.0),0.0),
                  (3.0,(-2.0),0.0)]

-- map funktion
myScale :: Float -> (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
myScale factor (x,y,z) = (factor * x,factor * y,factor *z)

-- kreise zeichen:
createCircle radius posX posY = circlePoints radius 100 posX posY

circlePoints :: Float -> Float -> GLfloat -> GLfloat -> [(GLfloat,GLfloat,GLfloat)]
circlePoints radius number posX posY = [let alpha = twoPi * i /number
                                 in ((radius*(sin (alpha)) + posX) ,(radius * (cos (alpha))+ posY),0.0)
                               |i <- [1,2..number]]
                               where
                                 twoPi = 2*pi



-- Baum zeichnen
drawBranches ::  [(GLfloat, GLfloat, GLfloat)] -> IO ()
drawBranches ((posX,posY,_):[]) = displayCircle 0.02 posX posY
drawBranches ((posX, posY,_):restPosListe) = do
                                            displayCircle 0.02 posX posY
                                            drawBranches restPosListe

displayCircle radius posX posY = do
  renderAs Polygon (createCircle radius posX posY)

renderAs figure pointlist = renderPrimitive figure$makeVertexes pointlist

makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

-- Hauuptprogramm
main = do
  getArgsAndInitialize
  meinWindow "Hello Baumi"
  mainLoop

meinWindow window_name = do
  createWindow window_name
  displayCallback $= drawTree

drawTree = do
  (drawBranches (map (myScale 0.1) branchesPoses))
  flush

