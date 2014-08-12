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
import Data.Set.CharSet
import Text.Groom


type Quote = String
type Named = String
data ExprR
	= ExRPostfix [Expr] String
	| ExRNamed String
	deriving (Eq,Read,Show,Ord)
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExSlot
	| ExBlock [Expr]
	| ExBranch [Expr]
	| ExInfixBinary Expr String Expr
	| ExPrefix String [Expr]
	| ExLR Expr ExprR
	| ExRegex
	-- | ExDumb
	deriving (Eq,Read,Show,Ord)

$(defineIsomorphisms ''ExprR)
$(defineIsomorphisms ''Expr)

operatorSymbols = stringCS "-+" --Read from the operator def table

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
symbol = subset (`elemCS` operatorSymbols) <$> token
operator = many1 symbol <|> many1 alpha



--identifier = cons <$> alpha <*> many (alpha <|> num)
identifier = many (alpha <|> num) 

mkSelector delim = 
	exSelector <$> pure d <*> between (text [d]) (text [d]) 
	(many $ subset (/=d) <$> token <|> (text [d,d] *> pure d))
	where
	d = delim

selectors = Ls.foldl1 (<|>) $ map mkSelector "/¶\\█○"

arityMark n = text $ replicate n '`'

preOp x = operator <*> text x

nList::(Syntax f, Eq a) => Int -> f a -> f [a]
nList n x = f n where
	f 0 = pure []
	f i = cons <$> x <*> f (i-1)
	
alts :: Alternative f => [f x] -> f x
alts = Ls.foldl1 (<|>)

expr::Syntax f => f Expr
expr = e 0 
	where
	postfixNRest x n = exRPostfix <$> nList (n-1) x <*> arityMark n *> operator
	e::Syntax f => Int -> f Expr
	e = \case
		i@0 -> chainl1 (e (i+1)) operator exInfixBinary
		i@1 -> let x = e $ i+1 in 
			(\r->foldl exLR <$> x <*> many r) 
			$	exRNamed <$> text "@" *> identifier
			<|>   alts (map (postfixNRest x) [1,2,3])
		_ -> expr'

expr'::Syntax f => f Expr
expr' = exRef <$> text "$" *> identifier
	<|> exSlot <$> text "_"
	<|> selectors
--	<|> exPrefix <$> text ":." <*> nlist 3 expr
--	<|> exPrefix <$> text ":" <*> nlist 2 expr
	<|> alts (map prefixN [1,2,3]) 
	<|> exBlock <$> between (text "{") (text "}") (many expr)
	where
	prefixN n = exPrefix <$> operator <*> arityMark n *> nList n expr 


test p p' x = do
	putStrLn "========================================="
	putStrLn x
	--let p0 = Parser.Parser p
	--let p1 = Printer.Printer p
	let a = Parser.parse p x::[Expr]
	putStrLn $ groom a
	let b = map (Printer.print p') a
	putStrLn $ unlines $ catMaybes b

main = do
	let t = test expr expr
	t "$abcf@kkk+++/.whatever//.kkk/@abc@def+++○div○@123@@"
	t "$aaa@kkk-_"
	t "--`$abcf"
	t "--``/.ww/@$"
	t "$a$b``+$``+"
	t "$1`+@5"
	t "$--$@$@$@```x/a/@``y/b/@``z-/fff/@5"

	t "--``$abcd"
	t ""
	t "$$"
	return ()

