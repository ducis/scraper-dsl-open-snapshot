#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}

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

--TODO: .html .text .etc
--http://api.jquery.com/category/attributes/
--	val() prop(String) html() hasClass(string) attr(string) text() css(String)
--TODO: Extraction
--TODO: Indentation

-- $a .`+ 
-- =
-- _ $a ``+

$(defineIsomorphisms ''Expr)
$(defineIsomorphisms ''Operator)

quote l r = between (text l) (text r)

operatorSymbols = stringCS "-+" --Read from the operator def table

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
symbol = subset (`elemCS` operatorSymbols) <$> token

operator f
	-- = opSimple <$> (many1 symbol <|> many1 alpha)
	= opSymbolic <$> many1 symbol
	<|> f (opAlphabetic <$> many1 alpha)
	<|> f (opComposed <$> exprList )

--identifier = cons <$> alpha <*> many (alpha <|> num)
identifier = many (alpha <|> num) 

mkSelector delim = 
	exSelector <$> pure d <*> quote [d] [d] 
	(many $ subset (/=d) <$> token <|> (text [d,d] *> pure d))
	where
	d = delim

exprList = quote "[" "]" (manySfx expr <* skipSpace)

selectors = Ls.foldl1 (<|>) $ map mkSelector "/¶\\█○"

arityMark n = text $ replicate n '`'

nList::(Syntax f, Eq a) => Int -> f a -> f [a]
nList n x = f n where
	f 0 = pure []
	f i = cons <$> x <*> f (i-1)
	
alts :: Alternative f => [f x] -> f x
alts = Ls.foldl1 (<|>)

naming = (text "@" ☆> identifier)
namesSfx = manySfx naming 

--infixr 6 <★>
infixr 6 <☆>
--a <★> b = a <*> optSpace *> b
a <☆> b = a <*> skipSpace *> b
--a ★> b = a *> optSpace *> b
a ☆> b = a *> skipSpace *> b
--many_ = (`sepBy` optSpace)
many' = (`sepBy` skipSpace)
manySfx = many.(skipSpace*>)
--manySfx_ = many.(sepSpace*>)

postfixNRest x n = nList (n-1) (x<*skipSpace) <*> arityMark n *> operator id <*> namesSfx
prefixN x n = exPrefix <$> operator id <*> arityMark n *> namesSfx <*> nList n (skipSpace*>x)

expr::Syntax f => f Expr
expr = e 0 
	where
	e::Syntax f => Int -> f Expr
	e = \case
		i@0 -> chainl1 (e (i+1)) (operator $ quote "`" "`") exInfixBinary
		i@1 ->
			foldl exPostfix <$> y <*> manySfx (alts $ map (postfixNRest y) [1,2,3])
			where
			x = e $ i+1 
			y = x <|> alts (map (prefixN x) [1,2,3])
		i@2 -> foldl exNamed <$> e(i+1) <*> namesSfx
		_ -> expr'

expr'::Syntax f => f Expr
expr' = exRef <$> text "$" *> identifier
	<|> exSlot <$> text "_"
	<|> selectors
	-- <|> alts (map prefixN [1,2,3]) 
	<|> exBlock <$> exprList

test p p' f x = do
	putStrLn "========================================="
	putStrLn x
	--let p0 = Parser.Parser p
	--let p1 = Printer.Printer p
	let a = Parser.parse p x
	--putStrLn $ groom a
	mapM_ f a
	let b = map (Printer.print p') a
	let ls = catMaybes b
	putStr $ unlines $ catMaybes b
	when (length ls /= 1) $ fail (show (length ls)++" results!")

main = do
	let t = test expr expr print
	let t' = test expr expr $ putStrLn.groom
	let t0 = test expr expr $ \_->return ()
	let z x s = putStr ">>" >> print s >> mapM_ print (Parser.parse x s)
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
	t0 "+``$1$2"
	t0 "$2$3``-"
	t0 "[+``$1$2]$3``-"
	t0 "+``$1[$2$3``-]"
	t0 "+``$1$2$3``-"
	t0 "+``@x$1@a$2@b$3@c$4@d```-@z"
	t0 "$`a`$"
	t0 "$a$``a"
	t' [here|$`[_`a]
	|]
	t' "$ $ ``+"
	t' "$a $ ``+"
	t' "$ $b ``+"
	t' "$a $b ``+"
	t' "$ $``+"
	t' "$$ ``+"

	t' "$ `+"
	t' "[ /a/ ]"
	t' "[/a/ ]"
	t' "[ /a/]"
	t' "[ ]"
	{-z exprList "[]"
	z exprList "[ ]"
	z (many' expr) "/a/"
	z (many' expr) "/a/ "
	z (many' expr) " /a/"
	z (many' expr) " /a/ "
	z (many' alpha) "a"
	z (many' alpha) "a "-}

	{-t "--``$abcd"
	t ""-}


	putStrLn "*******\nDONE!!!\n*******"
	return ()
