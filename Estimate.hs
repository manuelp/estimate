{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Estimate where

newtype Task = Task String deriving Show
newtype Time = Time Float deriving (Eq, Show, Num, Fractional, Ord)
data Estimate = Estimate Task Time Time Time deriving Show
data Eta = Eta Task Time Time

instance Show Eta where
    show (Eta t eta delta) = "ETA of " ++ show t ++ ": " ++ show eta ++ " +/- " ++ show delta

estimate :: Estimate -> Eta
estimate (Estimate t best normal worst) = (uncurry (Eta t)) (calcEstimate best normal worst)

calcEstimate :: Time -> Time -> Time -> (Time, Time)
calcEstimate best normal worst = (calcEta best normal worst, 
                                  calcDelta best normal)
    where calcEta a m b = (a + 4 * m + b) / 6
          calcDelta a b = (b - a) / 6
