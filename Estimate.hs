{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Estimate where

newtype Task = Task String deriving Show
newtype Time = Time Float deriving (Eq, Show, Num, Fractional, Ord)
data Estimate = Estimate {description :: Task, 
                          best :: Time,
                          normal :: Time,
                          worst :: Time}
              deriving Show
data Eta = Eta {task :: Task,
                estimation :: Time,
                delta :: Time}

instance Show Eta where
    show (Eta t e d) = "ETA of " ++ show t ++ ": " ++ show e ++ " +/- " ++ show d

estimate :: Estimate -> Eta
estimate (Estimate {description = t,
                    best = a,
                    normal = m,
                    worst = b}) = (Eta t e d) 
    where e = (a + 4 * m + b) / 6
          d = (b - a) / 6
