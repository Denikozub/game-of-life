module Indexing where

-- Get list of cell neighbours by its position in array and array size
neighbours :: Int -> Int -> [Int]
neighbours size pos = [
  (size^2 + pos - size - ((pos-size) `mod` size) + ((pos-size+1) `mod` size)) `mod` size^2, 
  (size^2 + pos - (pos `mod` size) + ((pos+1) `mod` size)) `mod` size^2, 
  (size^2 + pos + size - ((pos+size) `mod` size) + ((pos+size+1) `mod` size)) `mod` size^2, 
  (size^2 + pos - size*(pos `div` size) + (((pos+size) `div` size) `mod` size)*size) `mod` size^2, 
  (size^2 + ((pos + size - ((pos+size) `mod` size)) `mod` size^2) + ((pos+size-1) `mod` size)) `mod` size^2, 
  (size^2 + pos - (pos `mod` size) + ((pos-1) `mod` size)) `mod` size^2,
  (size^2 + pos - size - ((pos-size) `mod` size) + ((pos-size-1) `mod` size)) `mod` size^2,
  (size^2 + pos - size*(pos `div` size) + (((pos-size) `div` size) `mod` size)*size) `mod` size^2
  ]