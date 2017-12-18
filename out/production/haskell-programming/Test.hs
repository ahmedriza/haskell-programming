module Test (main) where

import Data.List    

main :: IO ()
main = do
    print "hello"
    let list = [1,2,3]
        list2 = map (+1) list

    return ()