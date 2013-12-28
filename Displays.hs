module Displays
(   simple,
    spirals,
    lineSpirals,
    bounce
) where

import Graphics.UI.GLUT
import MathCore

simple :: DisplayCallback
simple = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
        scaleFunc = id
        argFunc = id
        circle_dataSet = take 120 $ repeat 0.4
        spiral_dataSet = [0.0,0.004..1.0]
    clear [ColorBuffer]
    renderPrimitive Points $ do
        color3f 0.8 0.8 0.2
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc circle_dataSet
        color3f 0.2 0.8 0.8
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc spiral_dataSet
    flush

spirals :: DisplayCallback
spirals = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
        scaleFunc s v = s * v
        argFunc = id
        dataSet = [0.0,0.005..1.0]
    clear [ColorBuffer]
    renderPrimitive Points $ do
        mapM (\s -> (do color3f s s s ; mapM (\[x,y] -> vertex2f (scaleFunc s x) (scaleFunc s y)) $ wrapFunc argFunc dataSet )) [0.05,0.1..1.0]
    flush

lineSpirals :: DisplayCallback
lineSpirals = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
        scaleFunc s v = s * v
        argFunc = id
        dataSet = [0.0,0.005..1.0]
    clear [ColorBuffer]
    renderPrimitive Lines $ do
        mapM (\s -> (do color3f s s s ; mapM (\[x,y] -> do vertex2f (scaleFunc s x) (scaleFunc s y); vertex2f 0.0 0.0) $ wrapFunc argFunc dataSet)) [0.05,0.1..1.0]
    flush


bounceL :: [a] -> [a]
bounceL [] = []
bounceL [x] = [x]
bounceL xs = head xs : last xs : bounceL ((init . tail) xs)

bounceR :: [a] -> [a]
bounceR [] = []
bounceR [x] = [x]
bounceR xs = last xs : head xs : bounceR ((init . tail) xs)

bounce :: DisplayCallback
bounce = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
        scaleFunc = id
        argFunc = (\x -> x ** 2)
        dataSet = [0.0,0.004..1.0]
    clear [ColorBuffer]
    renderPrimitive Points $ do
        color3f 0.8 0.8 0.2
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc dataSet
        color3f 0.2 0.8 0.8
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc (reverse dataSet)
        color3f 0.7 0.3 0.0
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc (bounceL dataSet)
        color3f 0.0 0.7 0.3
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc (bounceR dataSet)
        color3f 0.9 0.4 0.9
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc ((reverse . bounceL) dataSet)
        color3f 0.9 0.9 0.4
        mapM (\[x,y] -> vertex2f (scaleFunc x) (scaleFunc y)) $ wrapFunc argFunc ((reverse . bounceR) dataSet)
    flush
