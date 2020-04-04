module File.FileModule(fuction) where

import System.IO
import Samplemodule
import Lib

fuction :: IO()
fuction = do
                  someFunc
                  handle <- openFile "hoge.txt" AppendMode 
                  hPutStrLn handle (sampleFun "test")
                  hPutStrLn handle (sampleFun "test2")
                  hClose handle