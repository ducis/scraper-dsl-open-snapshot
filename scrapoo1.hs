#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction #-}

import Text.Groom
import Text.Boomerang
import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Control.Monad
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH
import Data.Char


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
	| ExBlock [Expr]
	| ExBranch [Expr]
	-- | ExDumb
	deriving (Eq,Read,Show)

$(makeBoomerangs ''Expr)

expr :: StringBoomerang r (Expr :- r)
expr = 
	(	rExRef . lit "$" . name
	-- <> rExNamed . expr . lit "@" . name --left recursion!
	<> rExBlock . lit "{" . rList expr . lit "}"
	<> rExQuote . rList1 (satisfy (/='¶')) . opt (lit "¶")
	)
	where
	name = rList1 (satisfy isAlphaNum) -- Should I allow zero-length names?

data Foo 
	= Bar 
	| Baz Int Char 
	| Brackted Foo
	deriving (Eq,Show,Read)

$(makeBoomerangs ''Foo)

foo =
	(	rBar
	<> rBaz . "baz-" . int . "-" . alpha
	<> rBrackted . "(" . foo . ")"
	)

main = do
	let test p x = do
		putStrLn "========================================="
		putStrLn x
		let a@(Right e) = parseString p x
		print e
		let b@(Just s) = unparseString p e
		--print s
		putStrLn s
	let t = test expr
	t "\"1234\""
	t "$abcf"
	t "$"
	t "$$"
	t "\\abcf"
	test foo "((baz-2-c))"
	return ()


