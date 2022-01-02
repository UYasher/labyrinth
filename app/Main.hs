{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib (game, program)
import Text.RawString.QQ (r)

titleText :: String
titleText =
  [r|
 __          ___      .______   ____    ____ .______       __  .__   __. .___________. __    __  
|  |        /   \     |   _  \  \   \  /   / |   _  \     |  | |  \ |  | |           ||  |  |  | 
|  |       /  ^  \    |  |_)  |  \   \/   /  |  |_)  |    |  | |   \|  | `---|  |----`|  |__|  | 
|  |      /  /_\  \   |   _  <    \_    _/   |      /     |  | |  . `  |     |  |     |   __   | 
|  `----./  _____  \  |  |_)  |     |  |     |  |\  \----.|  | |  |\   |     |  |     |  |  |  | 
|_______/__/     \__\ |______/      |__|     | _| `._____||__| |__| \__|     |__|     |__|  |__| 
|]

main :: IO ()
main = do
  putStrLn titleText
  program game
  pure ()