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
import Data.String.Here
import Syntax.Slot


type Quote = String
type Name = String
data Operator 
	= OpSymbolic String 
	| OpAlphabetic String --Including abbreviation
	| OpComposed [Expr]
	deriving (Eq,Read,Show,Ord)
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExSlot
	| ExBlock [Expr]
	-- | ExBranch [Expr] 
	| ExInfixBinary Expr Operator Expr
	| ExPrefix Operator [Name] [Expr]
	| ExPostfix Expr [Expr] Operator [Name]
	| ExNamed Expr Name
	| ExRegex --TODO
	deriving (Eq,Read,Show,Ord)

--TODO: Spacing
--TODO: Extraction

-- $a .`+ 
-- =
-- _ $a ``+

$(defineIsomorphisms ''Expr)
$(defineIsomorphisms ''Operator)

quote l r = between (text l) (text r)
quote' l r = between (text l<*skipSpace) (skipSpace*>text r)

operatorSymbols = stringCS "-+" --Read from the operator def table

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
symbol = subset (`elemCS` operatorSymbols) <$> token

operator f
	-- = opSimple <$> (many1 symbol <|> many1 alpha)
	= opSymbolic <$> many1 symbol
	<|> f (opAlphabetic <$> many1 alpha)
	<|> f (opComposed <$> exprs)

--identifier = cons <$> alpha <*> many (alpha <|> num)
identifier = many (alpha <|> num) 

mkSelector delim = 
	exSelector <$> pure d <*> quote [d] [d] 
	(many $ subset (/=d) <$> token <|> (text [d,d] *> pure d))
	where
	d = delim

exprs = quote' "[" "]" (many' expr)

selectors = Ls.foldl1 (<|>) $ map mkSelector "/¶\\█○"

arityMark n = text $ replicate n '`'

nList::(Syntax f, Eq a) => Int -> f a -> f [a]
nList n x = f n where
	f 0 = pure []
	f i = cons <$> x <*> f (i-1)
	
alts :: Alternative f => [f x] -> f x
alts = Ls.foldl1 (<|>)

naming = (text "@" ☆> identifier)
names = many' naming 

infixr 6 <★>
infixr 6 <☆>
a <★> b = a <*> optSpace *> b
a <☆> b = a <*> skipSpace *> b
a ★> b = a *> optSpace *> b
a ☆> b = a *> skipSpace *> b
many_ = (`sepBy` optSpace)
many' = (`sepBy` skipSpace)

postfixNRest x n = nList (n-1) x <☆> arityMark n *> operator id <☆> names
prefixN x n = exPrefix <$> operator id <*> arityMark n ☆> names <☆> nList n x

expr::Syntax f => f Expr
expr = e 0 
	where
	e::Syntax f => Int -> f Expr
	e = \case
		i@0 -> chainl1 (e (i+1)) (operator $ quote "`" "`") exInfixBinary
		i@1 -> let x = e $ i+1 in 
			foldl exPostfix <$> x <☆> many' (alts $ map (postfixNRest x) [1,2,3])
			<|> alts (map (prefixN x) [1,2,3])
		i@2 -> foldl exNamed <$> e(i+1) <☆> names
		_ -> expr'

expr'::Syntax f => f Expr
expr' = exRef <$> text "$" *> identifier
	<|> exSlot <$> text "_"
	<|> selectors
	-- <|> alts (map prefixN [1,2,3]) 
	<|> exBlock <$> exprs

test p p' f x = do
	putStrLn "========================================="
	putStrLn x
	--let p0 = Parser.Parser p
	--let p1 = Printer.Printer p
	let a = Parser.parse p x::[Expr]
	--putStrLn $ groom a
	f a
	let b = map (Printer.print p') a
	putStr $ unlines $ catMaybes b

main = do
	let t = test expr expr print
	let t' = test expr expr $ putStrLn.groom
	let t0 = test expr expr $ \_->return ()
	t0 "$abcf@kkk+++/.whatever//.kkk/@abc@def+++○div○@123@1@1"
	t0 "$@+++/.whatever//.kkk/@@+++○div○@@@"
	t0 "$aaa@kkk-_"
	t0 "--`$abcf"
	t0 "--``/.ww/@1$1"
	t0 "$a$b``+$1``+"
	t0 "$1`+@5"
	t0 "+`$1@5"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y/b/@2``z-/fff/@5"
	t0 "$--$@$@$@```x/a/@``y/b/@``z-/fff/@"
	t0 "+``$1$2$3``-"
	t0 "+``@x$1@a$2@b$3@c$4@d```-@z"
	t0 "$`a`$"
	t0 "$a$``a"
	t' [here|$`[_`a]
	|]
	t' "$ `+"
	t' "$ $ ``+"
	t' "$a $ ``+"
	t' "$ $b ``+"
	t' "$a $b ``+"
	t' "$ $``+"
	t' "$$ ``+"

	t "--``$abcd"
	t ""
	return ()