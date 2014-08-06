{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           GHCJS.DOM.Document
import           GHCJS.DOM.Types
import           GHCJS.Foreign
import           GHCJS.Types
import           System.Mem


fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b : _)) = (a + b) : next t

foreign import javascript unsafe
  "document.getElementById(\"box2\").innerHTML = $1;"
  putString :: JSString -> IO ()

foreign import javascript unsafe
  "(function () { var box2 = document.createElement(\"div\"); box2.setAttribute(\"id\", \"box2\"); document.getElementsByTagName(\"body\")[0].appendChild(box2); })();"
  createBox :: IO ()

loop :: Int -> IO ()
loop i = do
    putString . toJSString . show $ fibs !! i
    performGC
    threadDelay 500000
    loop (i + 1)

main :: IO ()
main = do
    createBox
    loop 0

