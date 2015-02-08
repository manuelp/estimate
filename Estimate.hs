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
                amount :: Time,
                delta :: Time}

instance Show Eta where
    show (Eta t e d) = "ETA of " ++ show t ++ ": " ++ show e ++ " +/- " ++ show d

estimate :: Estimate -> Eta
estimate estimation = (Eta (description estimation) e d) 
    where e = ((best estimation) + 4 * (normal estimation) + (worst estimation)) / 6
          d = ((worst estimation) - (best estimation)) / 6
