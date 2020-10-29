module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import ClientSpec( clientTestTree)

main :: IO ()
main = do 
  task <- clientTestTree
  defaultMain $ testGroup "All" [task]