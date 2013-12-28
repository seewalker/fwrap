module MathCore
(   dotMult,
    mtrxMult,
    mtrxSpin,
    wrapFunc
) where

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
