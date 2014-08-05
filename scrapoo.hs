#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}

import Text.Groom
import Text.Boomerang
import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Control.Monad
import Data.Char
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Text.Syntax
import qualified Text.Syntax.Parser.Naive as Parser
import qualified Text.Syntax.Printer.Naive as Printer
import Data.Maybe


type Quote = String
type Named = String
data Expr 
	= ExSelector String
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
	deriving (Eq,Read,Show,Ord)

$(defineIsomorphisms ''Expr)

alpha = subset isAlpha <$> token
num = subset isNumber <$> token

identifier = cons <$> alpha <*> many (alpha <|> num)

expr::Syntax f => f Expr
expr = exRef <$> text "$" *> identifier
	<|> exBlock <$> betweenl (text "{")
	<|> exSelector <$> many1 (subset (/='¶') <$> token)

test p p' x = do
	putStrLn "========================================="
	putStrLn x
	--let p0 = Parser.Parser p
	--let p1 = Printer.Printer p
	let a = Parser.parse p x::[Expr]
	print a
	let b = map (Printer.print p') a
	putStrLn $ unlines $ catMaybes b


main = do
	let t = test expr expr
	t "\"1234\""
	t "$abcf"
	t "$"
	t "$$"
	t "\\abcf"
	return ()

