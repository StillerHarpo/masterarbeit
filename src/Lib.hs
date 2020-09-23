{-# language TupleSections #-}

module Lib where

saveIdx :: Eq a => a -> [a] -> Maybe Int
saveIdx = saveIdxH 0
  where
    saveIdxH _ _ []                 = Nothing
    saveIdxH i v (x:xs) | v == x    = Just i
                        | otherwise = saveIdxH (i+1) v xs

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(x:_)  !!? 0 = Just x
(_:xs) !!? n = xs !!? (n-1)

secondM :: (Functor m) => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = (a,) <$> f b
