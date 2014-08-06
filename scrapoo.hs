#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}

import Text.Groom
import Prelude hiding ((.), id, foldl)
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
import qualified Data.List as Ls


type Quote = String
type Named = String
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExComposed {
		-- ecFixity::Char{-'i','l','r'-}, --
		ecOperator::String, 
		ecOperands::[Expr]
		}
	| ExNamed {
		enExpr::Expr,
		enName::String 
		}
	| ExBlock [Expr]
	| ExBranch [Expr]
	| ExInfixBinary Expr String Expr
	-- | ExDumb
	deriving (Eq,Read,Show,Ord)

$(defineIsomorphisms ''Expr)

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
symbol = subset isSymbol <$> token
symbolicOp = many1 symbol

--identifier = cons <$> alpha <*> many (alpha <|> num)
identifier = many (alpha <|> num) 

expr::Syntax f => f Expr
expr = e0 
	where
	e0 = chainl1 e1 symbolicOp exInfixBinary
	e1 = foldl exNamed <$> e2 <*> many (text "@" *> identifier)
	e2 = expr'

mkSelector delim = 
	exSelector <$> pure d <*> between (text [d]) (text [d]) 
	(many $ subset (/=d) <$> token <|> (text [d,d] *> pure d))
	where
	d = delim

selectors = Ls.foldl1 (<|>) $ map mkSelector "/¶\\█○"
--selectors = mkSelector '/' <|> mkSelector '\\'


expr' = exRef <$> text "$" *> identifier
	<|> exBlock <$> between (text "{") (text "}") (many expr)
	<|> selectors

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
	t "$abcf@kkk+++/.whatever//.kkk/@abc@def+++○div○@123@@"
	t "$aaa@kkk"
	t ""
	t "$$"
	return ()

