module Utils.Utils 
 ( getFix
 , isInt
 , showRat
 , iterN
 , setNub
 , copy
 , rounder
 , roundDown
 , roundUp) where
import qualified Data.List as L
import qualified Data.Set as S

setNub :: Ord a => [a] -> [a]
setNub = S.toAscList . S.fromList

getFix :: Eq a => (a -> a) -> a -> a
getFix f a 
   | new == a  = a
   | otherwise = getFix f new
      where new = f a

isInt :: Rational -> Bool
isInt t = (fromIntegral (round t :: Integer)) == t

showRat :: Rational -> String
showRat r = 
   if isInt r 
   then show (round r :: Integer) 
   else show r 

iterN :: Int -> (a -> a) -> a -> a
iterN n f = last . take (n + 1) . L.iterate f 

copy :: a -> a
copy = id

rounder :: RealFrac a => Int -> a -> Rational
rounder k = (/10^k) . round' . (* 10^k) 
   where 
      round' b = fromIntegral $ 
                 if abs r < 0.5 
                 then n 
                 else n + signum n 
         where (n, r) = properFraction b

roundDown :: RealFrac a => Int -> a -> Rational
roundDown k = (/10^k) . fromIntegral . floor . (* 10^k) 

roundUp :: RealFrac a => Int -> a -> Rational
roundUp k = (/10^k) . fromIntegral . ceiling . (* 10^k) 

