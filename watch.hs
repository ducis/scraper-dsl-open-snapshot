import Control.Monad
import Data.List
import System.Process
import System.Environment

main = do
	[file,cmd]<-getArgs
	getContents >>= 
		mapM_ (\_->spawnCommand cmd>>=waitForProcess>>putStrLn "\n✠✠✠✠✠✠✠✠✠✠✠\n✠✠✠✠✠✠✠✠✠✠✠\n")
		.filter (isSuffixOf $ "CLOSE_WRITE,CLOSE "++file)
		.lines
	--getContents >>= mapM_ putStrLn.lines
