import Control.Monad
import Data.List
import System.Process
main = do
	getContents >>= 
		mapM_ (\_->spawnCommand "make">>=waitForProcess>>putStrLn "✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠✠")
		.filter (isSuffixOf "CLOSE_WRITE,CLOSE scrapoo.hs")
		.lines
	--getContents >>= mapM_ putStrLn.lines
