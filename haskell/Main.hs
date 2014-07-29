
import           Control.Concurrent (threadDelay)
import           Control.Monad
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
  "updateDOMs();"
  updateDOMs :: IO ()

loop :: Int -> IO ()
loop i = do
    putString . toJSString . show $ fibs !! i
    performGC
    updateDOMs
    threadDelay 500000
    loop (i + 1)

main :: IO ()
main = loop 0

