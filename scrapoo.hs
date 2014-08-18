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
import qualified Data.List.Split as LS
import Debug.Diff

skipChars s = ignore [] <$> many (Ls.foldl1 (<|>) [text [c]|c<-s])
sW = skipChars " \t\r\n"
sw = skipChars " \t"

type Quote = String
type Name = String
data Operator 
	= OpSymbolic String 
	| OpAlphabetic String --Including abbreviation
	| OpComposed Char [Expr]
	deriving (Eq,Read,Show,Ord)
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExSlot
	| ExBlock Char [Expr]
	-- | ExBranch [Expr] 
	| ExLeftRec Expr LeftRecRest
	-- | ExInfix Expr Operator [Expr]
	| ExCurriedLeft Operator [Expr]
	| ExPrefix Operator [Name] [Expr]
	-- | ExPostfix Expr [Expr] Operator [Name]
	| ExNamed Expr Name
	| ExRegex --TODO
	deriving (Eq,Read,Show,Ord)
data LeftRecRest
	= LrrInfix Operator [Name] [Expr]
	| LrrPostfix [Expr] Operator [Name]
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
$(defineIsomorphisms ''LeftRecRest)

quote l r = between (text l) (text r)

operatorSymbols = stringCS "-+" --Read from the operator def table

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
char c = subset (==c) <$> token
charset cs = subset (`elemCS` cs) <$> token
symbol = charset operatorSymbols --subset (`elemCS` operatorSymbols) <$> token

operator f
	-- = opSimple <$> (many1 symbol <|> many1 alpha)
	= opSymbolic <$> many1 symbol
	<|> f (opAlphabetic <$> many1 alpha)
	<|> f (opComposed <$> exprList )

--identifier = cons <$> alpha <*> many (alpha <|> num)
--TODO: allow arbitrary string literal
identifier 
	= pure []
	<|> cons <$> x <*> pure []
	<|> cons <$> x' <*> many1 x'
	where
	x = alpha<|>num
	x' = x<|>char '_'

mkSelector delim = 
	exSelector <$> pure d <*> quote [d] [d] 
	(many $ subset (/=d) <$> token <|> (text [d,d] *> pure d))
	where
	d = delim

sepBy1::(Syntax f, Eq a) => f a -> f () -> f [a]
sepBy1 x d = cons <$> x <*> many (d*>x)

exprList::Syntax f => f (Char,[Expr])
exprList 
	= pure ';' <*> quote "[" "]" (manySfx (expr<☆text ";") <* sW)
	-- = text "[" ☆> pure [] <*text "]"
	<|> pure ',' <*> quote "[" "]" (sW*>sepBy1 expr delim<*sW)
	where
	delim 
		= sW <* text "," <* sW
	--		<|> sw <* text "\n" <* sW
--TODO: lack of delimiters makes parser unacceptablly slow

selectors = Ls.foldl1 (<|>) $ map mkSelector "/¶\\█○"


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
a <☆> b = a <*> sW *> b
--a ★> b = a *> optSpace *> b
a ☆> b = a *> sW *> b
a <☆ b = a <* sW <* b
--many_ = (`sepBy` optSpace)
many' = (`sepBy` sW)
manySfx = many.(sW*>)
many1Sfx = many1.(sW*>)
--manySfx_ = many.(sepSpace*>)

arityMark n = text $ replicate n '`'
postfixNRest x n = nList (n-1) (x<*sW) <*> arityMark n *> operator id <*> namesSfx
prefixN x n = exPrefix <$> operator id <*> arityMark n *> namesSfx <*> nList n (sW*>x)
infixOperator = \case
	0 -> operator (quote "`" "`") 
	j -> text "`" *> operator id <* arityMark (j+1)

--TODO:currying (not left recursive)
expr::Syntax f => f Expr
expr = e 0 
	where
	e::Syntax f => Int -> f Expr
	e i = [
		let 
			r 0 = infixOperator 0 <*> namesSfx <☆> nList 1 x 
			r j = infixOperator j <*> namesSfx <*> (cons <$> (sW*>self) <*> nList j (sW*>x))
			lrRest
				=		lrrInfix <$> (r 0 <|> r 1)
				<|>	lrrPostfix <$> (alts $ map (postfixNRest self) arities)
		in foldl exLeftRec <$> x <*> manySfx lrRest
		,
		{-let
			infiX = foldl exInfix <$> x <*> manySfx rest
			rest
				= infixOperator 0 <☆> nList 1 x
				<|> r 1
				where
				r j = infixOperator j <*> (cons <$> (sW*>infiX) <*> nList j (sW*>x))
		in infiX 
		,
		let 
			postfiX = foldl exPostfix <$> x <*> manySfx (alts $ map (postfixNRest postfiX) arities)
		in postfiX
		,-}
		x <|> alts (map (prefixN self) arities)
		,
		foldl exNamed <$> x <*> namesSfx
		,
		expr'
		] !! i
		where
		arities = [1,2,3]
		x = e (i+1)
		self = e i

expr'::Syntax f => f Expr
expr' = exRef <$> text "$" *> identifier
	<|> exSlot <$> text "_"
	<|> selectors
	-- <|> alts (map prefixN [1,2,3]) 
	<|> exBlock <$> exprList
	-- <|> exCurriedLeft <$> alts [infixOperator i <*> 

snippet = between sW sW expr

test p p' f x = do
	putStrLn "========================================="
	putStrLn x
	let a = Parser.parse p x
	let na = Ls.nub a
	let 
		df (x:y:zs) = diff x y>>f y>>df (y:zs)
		df _ = return ()
	case na of 
		(x:xs) -> f x >> df na
		_ -> return ()
	let b = map (Printer.print p') $ na
	let ls = Ls.nub $ catMaybes b
	let l = length
	putStr $ unlines $ ls
	when (l a /= 1 || l ls /=1) $ fail (show (l a)++","++show (l ls)++" results!")


main = do
	let _t = test snippet snippet $ putStrLn.groom
	let _t0 = test snippet snippet $ \_->return ()
	let t = test expr expr print
	let t' = test expr expr $ putStrLn.groom
	let t0 = test expr expr $ \_->return ()
	let z x s = putStr ">>" >> print s >> mapM_ print (Parser.parse x s)
	t0 "$ - $"
	t0 "$`-``$$"
	t0 "$`-``[$`-``$$][$`-``$$]"
	t0 "$a`-`` $b`-``$c$d $e`-``$f$g"
	t0 "$`-`` $ $`-``$$"
	t0 "$`-`` $`-``$$ $"
	t0 "$ $ $ ```-"
	t0 "-``` $ $ $"
	t0 "//@"
	t0 "//@a_b"
	t0 "//@a_"
	t0 "//@_b"
	t0 "$abcf@kkk+++/.whatever//.kkk/@abc@def+++○div○@123@1@1"
	t0 "$@+++/.whatever//.kkk/@@+++○div○@@@"
	t0 "$aaa@kkk-_"
	t0 "--`$abcf"
	t0 "--``/.ww/@1$1"
	t0 "$a$b``+$1``+"
	t0 "$1`+@5"
	t0 "+`$1@5"
	t0 "$1--$2@3"
	t0 "$1--$2@3$4@5$5@6```x"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y"
	t0 "$1--$2@3$4@5$5@6```x/a/@1``y/b/@2``z"
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

	let tf f = readFile f >>= mapM_ _t.map unlines.LS.splitOn ["==="].lines
	tf "sampletests"
	
	t0 "-``[-``$$][-``$$]"
	t0 "-`` [-``$$] -``$$"
	t0 "-`` -``$$ [-``$$]"
	t0 "-``-``$$ [-``$$]"
	t0 "-`` -``$$ -``$$"
	t0 "-``-``$$ -``$$"
	t0 "[$$``-][$$``-]``-"
	t0 "$$``- [$$``-] ``-"
	t0 "[$$``-] $$``- ``-"
	t0 "[$$``-] $$``-``-"
	t0 "$$``- $$``- ``-"
	t0 "$$``- $$``-``-"

	t' "$-$-$`children"
	t' "$1-$2-$3"
	t' "$1-$2$3``-"
	t' "$1-$2`-"

	t' "$`[-$a@x,+$b@y,`find`$c@z, +$+//@+//@]"

	tf "sampletests1"

	putStrLn "*******\nDONE!!!\n*******"
	return ()
