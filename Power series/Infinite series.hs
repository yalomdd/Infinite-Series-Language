{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
import qualified Data.Map as M
import           Data.Complex
import           Data.Ratio
import           Text.Printf

-- array representation of a polynomial
type Poly a = [a]

-- z is representing the real unit in a power series form
z :: Num a => Poly a
z = [0,1] ++ repeat 0

-- i is represting the imaginary unit in a power series form
i :: RealFloat a => Poly (Complex a)
i = [0 :+ 1] ++ repeat 0

-- factorial helper function
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- this represents the power series of e ^ m
e :: Fractional a => a -> Poly a
e m = map (\n -> (m ^ n) / fromIntegral (fact n)) [0..]

--this represents cosin as a power series
co :: Fractional a =>  Poly a
co = map f [0..]
  where
    f :: Fractional a => Integer -> a
    f n
      | n `mod` 4 == 0 = (1 / fromIntegral (fact n) )
      | n `mod` 4 == 2 = (-1 / fromIntegral (fact n) )
      | otherwise      = 0
-- this represents sin as a power seires
si :: Fractional a =>  Poly a
si = -(d co)

-- d is the derivitave function
d :: Num a => Poly a -> Poly a
d (x:xs) = de (*) 1 xs

-- int is the integral function
int :: Fractional a => Poly a -> Poly a
int xs = 1 : de (/) 1 xs

-- de is a helper function to compute integrals and derivates
de :: Num a => (a -> a -> a) -> a -> Poly a -> Poly a
de f n []     =  []
de f n (x:xs) =  f n x : de f (n + 1) xs

-- this defines the cauchy product of two power series
-- https://en.wikipedia.org/wiki/Cauchy_product
cauchyProd :: Num a => Poly a -> Poly a -> Poly a
cauchyProd (x:xs) b@(y:ys) = x * y : map (\a -> a * x) ys + xs * b

-- defines division between power series
ddiv :: Fractional a => Poly a -> Poly a -> Poly a
ddiv (x:xs) (y:ys) = qs
  where
    qs = x / y : map (\x -> x / y) (xs - qs * ys)

-- # defines the functional composition of two power series
(#) :: Num a => Poly a -> Poly a -> Poly a
(x:xs) # g@(y:ys) = x : (ys * (xs # g))

-- allows us to inherit the numeric interface
instance Num a => Num (Poly a) where
    (+)           = zipWith (+)
    (-)           = zipWith (-)
    (*)           = cauchyProd
    abs           = map abs
    signum        = map signum
    fromInteger n = [fromInteger n,0] ++ repeat 0
    
-- allows us to inherit the fractional interface
instance Fractional a => Fractional (Poly a) where
   fromRational n = [fromRational n,0] ++ repeat 0
   (/)            =  ddiv
  
-- the rest defines and builds pretty printing defines pretty printing
class Pretty a where
  pretty, sign :: a -> String

instance (Pretty a, Num a , Eq a) => Pretty (Complex a) where
  pretty = pComplex
  sign _ = "+"

instance Pretty Integer where
  pretty = show . abs
  sign   = orSign

instance Pretty Double where
   pretty = printf "%.2f" . abs
   sign   = orSign

instance (Pretty a, Integral a , Ord a) => Pretty (Ratio a) where
  pretty = pRatio
  sign   = orSign


orSign :: (Num a, Ord a) => a -> String
orSign a = if (a < 0) then "-" else "+"


pComplex :: (Pretty a, Num a , Eq a) => Complex a -> String
pComplex (n :+ 0) = pretty n
pComplex (0 :+ n) = pretty n ++ "i"
pComplex (n :+ m) = "(" ++ pretty n ++ sign m ++ pretty m ++ "i" ++ ")"

pRatio :: (Pretty a, Num a , Eq a) => Ratio a -> String
pRatio r =
  case denominator r of
    1  -> pretty (numerator r)
    j  -> "(" ++ pretty (numerator r) ++ "/" ++ pretty j ++ ")"

pTake :: (Pretty a, Eq a, Num a) => Integer -> Poly a -> String
pTake n pol = concat (map p (zip (take (fromInteger n) pol) [0..]))
  where
    p :: (Pretty a, Eq a, Num a) => (a , Integer) -> String
    p (0, _) = ""
    p (n,0) = if sign n == "-" then sign n ++ pretty n else pretty n
    p (n,1) = sign n ++ pretty n ++ "z"
    p (n,m) = sign n ++ pretty n ++ "z^" ++ show m
