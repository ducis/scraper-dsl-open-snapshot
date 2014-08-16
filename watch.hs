{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
import Control.Monad
import Data.List
import System.Process
import System.Environment
import System.Path.WildMatch
import Text.Regex.PCRE.Rex

main = do
	(cmd:globs)<-getArgs
	putStrLn $ "Watching "++unwords globs
	let f s = Just True ==
		[rex|CLOSE_WRITE,CLOSE\ (?{\path->[]/=filter (`wildCheckCase`path) globs}.*)|] s
		--isInfixOf "CLOSE_WRITE,CLOSE " s && breakList

	getContents >>= 
		mapM_ (\_->spawnCommand cmd>>=waitForProcess>>putStrLn "\n✠✠✠✠✠✠✠✠✠✠✠\n✠✠✠✠✠✠✠✠✠✠✠\n")
		-- .filter (isSuffixOf $ "CLOSE_WRITE,CLOSE "++file)
		.filter f
		.lines
	--getContents >>= mapM_ putStrLn.lines
