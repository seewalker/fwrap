import Graphics.UI.GLUT

dotMult :: Num a => [a] -> [a] -> a
dotMult x y = sum $ zipWith (*) x y

mtrxMult :: Num a => [[a]] -> [a] -> [a]
mtrxMult m x = map (dotMult x) m

--only to be used with vectors in R2
mtrxSpin :: Floating a => a -> [a] -> [a]
mtrxSpin theta vec = mtrxMult [[cos theta, -1 * sin theta], [sin theta, cos theta]] vec

wrapFunc f inds = wrapFunc' f inds 0.0 (fromIntegral (length inds))
    where wrapFunc' f [] pos len = []
          wrapFunc' f inds pos len = (mtrxSpin (theta_i pos len) $ [(head inds),(f . head) inds]) : wrapFunc' f (tail inds) (pos + 1) len
            where theta_i p l = 2 * pi * (p / l)

--the type of renderPrimitive is `PrimitiveMode -> IO a -> IO a
--  Points is the value of type PrimitiveMode
--  the do block is the value of type IO. Everything in the do block is of type IO.
displaySimple :: DisplayCallback
displaySimple = do
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

displaySpirals :: DisplayCallback
displaySpirals = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
        scaleFunc s v = s * v
        argFunc = id
        dataSet = [0.0,0.005..1.0]
    clear [ColorBuffer]
    renderPrimitive Points $ do
        mapM (\s -> (do color3f s s s ; mapM (\[x,y] -> vertex2f (scaleFunc s x) (scaleFunc s y)) $ wrapFunc argFunc dataSet )) [0.05,0.1..1.0]
    flush

bounceL :: [a] -> [a]
bounceL [] = []
bounceL [x] = [x]
bounceL xs = head xs : last xs : bounceL ((init . tail) xs)

bounceR :: [a] -> [a]
bounceR [] = []
bounceR [x] = [x]
bounceR xs = last xs : head xs : bounceR ((init . tail) xs)

displayBounce :: DisplayCallback
displayBounce = do
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

--In all use cases so far, it turns out that I do not need a reshape callback
--for my windowing system to reshape the drawing window.
--($=) :: HasSetter s => s a -> a -> IO ()
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Rotating Functions Around A Circle"
    displayCallback $= displaySpirals
    mainLoop
