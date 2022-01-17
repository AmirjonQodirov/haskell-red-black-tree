module Main where

import Lib


prop_addTree a b c d tree = Lib.put a (Lib.put b (Lib.put c (Lib.put d tree)))
prop_addTree2 a tree = Lib.put a tree
prop_addTree3 a tree = Lib.delete a tree

main = do 
    let p1 = prop_addTree2 0 empty 
    let p2 = prop_addTree3 0 p1
    putStrLn (show (p2))
