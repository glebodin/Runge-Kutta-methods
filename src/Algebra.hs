module Algebra
    ( VectorField(..)
    )
where

class VectorField f where
    add :: Num a => f a -> f a -> f a
    dot :: Num a => f a -> f a -> a
    mul :: Num a => a -> f a -> f a

instance VectorField [] where
    add = zipWith (+)
    dot a = sum . zipWith (*) a
    mul x = fmap (* x)
