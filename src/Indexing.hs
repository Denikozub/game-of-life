module Indexing where

-- Get list of cell neighbours by its position in array and array size
neighbours :: Int -> Int -> [Int]
neighbours size pos = (horisontal size pos) ++ (vertical size pos) ++ (left_bottom size pos) ++
  (right_bottom size pos) ++ (left_upper size pos) ++ (right_upper size pos)

horisontal :: Int -> Int -> [Int]
horisontal n x | x `mod` n == 0 = [x + 1, x + n - 1]
               | x `mod` n == n - 1 = [x - 1, x - n + 1]
               | otherwise = [x - 1, x + 1]

vertical :: Int -> Int -> [Int]
vertical n x | x `div` n == 0 = [x + n, n * (n - 1) + x]
             | x `div` n == n - 1 = [x - n, x `mod` n]
             | otherwise = [x - n, x + n]

left_bottom :: Int -> Int -> [Int]
left_bottom n x | x `mod` n /= 0 = [(x + n - 1) `mod` (n * n)]
                | otherwise = [(x + 2 * n - 1) `mod` (n * n)]

right_bottom :: Int -> Int -> [Int]
right_bottom n x | x `mod` n /= n - 1 = [(x + n + 1) `mod` (n * n)]
                 | otherwise = [(x + 1) `mod` (n * n)]

left_upper :: Int -> Int -> [Int]
left_upper n x | x `mod` n /= 0 = [(x - n - 1 + n * n) `mod` (n * n)]
               | otherwise = [(x - 1 + n * n) `mod` (n * n)]

right_upper :: Int -> Int -> [Int]
right_upper n x | x `mod` n /= n - 1 = [(x - n + 1 + n * n) `mod` (n * n)]
                | otherwise = [(x - 2 * n + 1 + n * n) `mod` (n * n)]
