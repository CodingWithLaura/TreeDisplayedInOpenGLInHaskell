module OpenGLTools where
import Graphics.UI.GLUT


scalePos :: GLfloat -> (Int,Int) ->  (GLfloat,GLfloat)
scalePos fac (x,y) = ((fac * (fromIntegral x)) , (fac * (fromIntegral y)) )

displayCircleWrap (x,y) = displayCircle 0.1 x y

displayCircle radius posX posY = do
  renderAs Polygon (createCircle radius posX posY)

renderAs figure pointlist = renderPrimitive figure$makeVertexes pointlist

makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

createCircle radius posX posY = circlePoints radius 100 posX posY

circlePoints :: Float -> Float -> GLfloat -> GLfloat -> [(GLfloat,GLfloat,GLfloat)]
circlePoints radius number posX posY = [let alpha = twoPi * i /number
                                 in ((radius*(sin (alpha)) + posX) ,(radius * (cos (alpha))+ posY),0.0)
                               |i <- [1,2..number]]
                               where
                                 twoPi = 2*pi
