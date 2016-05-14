module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-i.", "Player.hs", "PlayerService.hs", "GameState.hs", "Hands.hs"]
