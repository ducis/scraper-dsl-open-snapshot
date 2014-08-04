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
import Data.Char

data Foo = Bar | Baz Int Char deriving (Eq,Show,Read)

type Quote = String
type Named = String
data Expr 
	= ExQuote String
	| ExRef String
	| ExComposed {
		ecFixity::Char{-'i','l','r'-}, 
		ecOperator::String, 
		ecOperands::[Expr]
		}
	| ExNamed {
		enExpr::Expr,
		enName::String
		}
	| ExBranch [Expr]
	-- | ExDumb
	deriving (Eq,Read,Show)

$(makeBoomerangs ''Expr)

expr :: StringBoomerang () (Expr :- ())
expr = 
	(	rExRef . lit "$" . rList1 (satisfy isAlphaNum) -- Should I allow zero-length names?
	<>	rExRef . lit "\\" . rList1 (satisfy isAlphaNum)
	<> rExQuote . rList (satisfy (/='¶')) . opt (lit "¶")
	)

main = do
	let test s = do
		putStrLn "========================================="
		let a@(Right e) = parseString expr s
		print e
		let b@(Just s) = unparseString expr e
		print s
		putStrLn s
	test "\"1234\""
	test "$abcf"
	test "$"
	test "$$"
	return ()
