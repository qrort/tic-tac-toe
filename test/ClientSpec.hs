module ClientSpec
  ( clientTestTree
  ) where

import Lens.Micro
import Test.Tasty(TestTree)
import Test.Tasty.Hspec(Spec, describe, it, shouldBe, testSpec)

import BoardUtils
import Client
import GameType
import Types

clientTestTree :: IO TestTree 
clientTestTree = testSpec "client spec" clientSpec

testBoard :: Board
testBoard = Board { _sz = 3
                  , _objects = [                                                                                           
                      Cell {_ctype = Circle, _coord = Pos {_x = 2, _y = 1}},                                                                 
                      Cell {_ctype = Cross, _coord = Pos {_x = 0, _y = 2}},                                                                  
                      Cell {_ctype = Circle, _coord = Pos {_x = 0, _y = 1}},                                                                 
                      Cell {_ctype = Cross, _coord = Pos {_x = 1, _y = 1}},                                                                  
                      Cell {_ctype = Circle, _coord = Pos {_x = 2, _y = 2}},                                                                 
                      Cell {_ctype = Cross, _coord = Pos {_x = 2, _y = 0}}
                    ]
                  }  

testGame :: Game
testGame = Game { _selection  = Pos 1 1
                , _board      = testBoard
                , _result     = Unknown
                , _playerCell = Cross
                , _aiCell     = Circle
                , _row        = 3 
                }

fullBoard :: Board
fullBoard = Board { _sz = 2
                  , _objects = [                                                                                           
                      Cell {_ctype = Circle, _coord = Pos {_x = 0, _y = 0}},                                                                 
                      Cell {_ctype = Cross, _coord = Pos {_x = 0, _y = 1}},                                                                  
                      Cell {_ctype = Circle, _coord = Pos {_x = 1, _y = 0}},                                                                 
                      Cell {_ctype = Cross, _coord = Pos {_x = 1, _y = 1}}                                                                 
                    ]
                  }  
fullGame :: Game
fullGame = testGame & board .~ fullBoard
  
clientSpec :: Spec
clientSpec = do
  describe "BoardTest" $ do
    it "playerWon" $
      (setOverResult (testGame))^.result `shouldBe` Victory
    it "playerLost" $
      (setOverResult (testGame & playerCell .~ Circle & aiCell .~ Cross))^.result `shouldBe` Loss
    it "gameDrawn" $
      (setDrawResult fullGame)^.result `shouldBe` Draw