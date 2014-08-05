#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings #-}

import Text.Groom
import Text.Boomerang
import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Control.Monad
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH

data Foo = Bar | Baz Int Char deriving (Eq,Show,Read)

$(makeBoomerangs ''Foo)

foo :: StringBoomerang () (Foo :- ())
foo =
	(	rBar
	<> rBaz . "baz-" . int . "-" . alpha
	)

test1 = parseString foo "baz-2-c"

test2 = parseString foo ""

test3 = parseString foo "baz-2-3"

test4 = unparseString foo (Baz 1 'z')

main = do
	mapM_ print $ [test1,test2,test3]
	print test4
	--print test4
