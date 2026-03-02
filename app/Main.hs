{-# LANGUAGE InstanceSigs #-}
module Main where
import GHC.Arr
import Text.Printf


main :: IO ()
main = mapM_ (\n -> printf "%d: %.2f%%\n" n (p1Win n * 100)) [2..50]


newtype Probability a 
  = Probability { getProbabilities :: [(a,Double)] }
    deriving Show

instance Functor Probability where
    fmap :: (a -> b) -> Probability a -> Probability b
    fmap f (Probability xs) = Probability $ map (\(x,p) -> (f x, p)) xs 
    
instance Applicative Probability where
    pure :: a -> Probability a 
    pure x = Probability [(x, 1.0)]
    (<*>) :: Probability (a -> b) -> Probability a -> Probability b
    (Probability fs) <*> (Probability xs) = Probability $ do
        (f, p1) <- fs 
        (x, p2) <- xs 
        pure (f x, p1 * p2)

instance Monad Probability where 
    (>>=) :: Probability a -> (a -> Probability b) -> Probability b
    Probability xs >>= f = Probability $ do 
        (x, p1) <- xs 
        let (Probability ys) = f x 
        (y, p2) <- ys 
        pure (y, p1 * p2)

roll :: Probability Int
roll = Probability [ (i,1/6) | i <- [1..6] ]

rollDice :: Int -> Probability Int 
rollDice 0 = pure 0
rollDice n = normalize $ do 
    current <- rollDice (n - 1)
    next <- roll 
    pure (current + next)


normalize :: (Ord a) => Probability a -> Probability a
normalize (Probability xs) = Probability $
    let grouped = foldr insert [] xs
        insert (x,p) [] = [(x,p)]
        insert (x,p) ((y,q):ys)
            | x == y = (y, p + q) : ys
            | otherwise = (y,q) : insert (x,p) ys
    in grouped

rollingStones :: Array Int (Probability Int) 
rollingStones = listArray (0, 50) [ stones n | n <- [0..50] ]
    where 
        stones 0 = pure 0 
        stones n = normalize $ do
            r <- roll
            let remaining = max 0 (n - r)
            if remaining == 0
                then pure 1
                else do 
                    next <- rollingStones ! remaining
                    pure (next + 1)

p1Win :: Int -> Double 
p1Win n = sum [ p | (k, p) <- getProbabilities (rollingStones ! n), odd k ]