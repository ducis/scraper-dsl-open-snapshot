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
import System.Environment
import DSL.Scrapoo.ParseTree

skipChars s = ignore [] <$> many (Ls.foldl1 (<|>) [text [c]|c<-s])
sW = skipChars " \t\r\n"
sw = skipChars " \t"

--http://api.jquery.com/category/attributes/
--	val() prop(String) html() hasClass(string) attr(string) text() css(String)
--	TODO: XPath
--	TODO Stanford tregex / TGrep2
--TODO: Indentation after pretty-printing
--TODO: Parallelize tests
--TODO: Type checker
--TODO: currying $$``[__``a] $$$```[___```b]
--		$$[``a] $$$[```b]
--TODO: numeric indexing and ranges
--TODO: code generation		CURRENT
--		SUB-TODO: operator table
--		SUB-TODO: assertion(x.length == 1);
--		/a/{`aid}@zzz 
--		translates to 
--		output.zzz = $('a').map(function(x){
--			var set = $(x);
--			assert(set.length == 1);
--			return $(x).attr(id);
--		});

--DONE; map-branching (both as an unary operator and as an operand). Just use curly braces

$(defineIsomorphisms ''Expr)
$(defineIsomorphisms ''Operator)
$(defineIsomorphisms ''LeftRecRest)
$(defineIsomorphisms ''Name)

quote l r = between (text l) (text r)

operatorSymbols = stringCS "-+" --Read from the operator def table

alpha = subset isAlpha <$> token
num = subset isNumber <$> token
char c = subset (==c) <$> token
charListed cs = subset (`elem` cs) <$> token
charset cs = subset (`elemCS` cs) <$> token
symbol = charset operatorSymbols --subset (`elemCS` operatorSymbols) <$> token

operator f
	-- = opSimple <$> (many1 symbol <|> many1 alpha)
	= opSymbolic <$> many1 symbol
	<|> f (opAlphabetic <$> many1 alpha)
	<|> f (opComposed <$> exprList)

prefixOperator n = operator id <* arityMark n
infixOperator = \case
	0 -> operator (quote "`" "`") 
	j -> text "`" *> operator id <* arityMark (j+1)
postfixOperator n = case n of
	1 -> x <|> opComposed <$> curriedList
	_ -> x
	where x = arityMark n *> operator id

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

braced = quote "(" ")"
crlBrcd = quote "{" "}"
sqBrktd = quote "[" "]" 
multiQuote pairs x = alts [char l <*> x <* text r|l:r<-pairs]

exprListBrackting = multiQuote ["[]","{}"]

curriedList,exprList :: Syntax f => f (Char, (Char, [Expr]))
curriedList = exprListBrackting $ bigList curried
exprList = exprListBrackting $ bigList expr

bigList::(Syntax f,Eq a) => f a -> f (Char,[a])
bigList x
	= pure ';' <*> (manySfx (x<☆text ";") <* sW)
	-- = text "[" ☆> pure [] <*text "]"
	<|> pure ',' <*> (sW*>sepBy1 x delim<*sW)
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

--naming = (text "@" ☆> identifier)
naming = name <$> charListed "@#" <☆> optional (accessor <☆ text ":" <*sW) <*> identifier <*> optional (sW*>text "^"☆>accessor)
	where
	accessor = many1 alpha
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
postfixNRest x n = nList (n-1) (x<*sW) <*> postfixOperator n <*> namesSfx
prefixN x n = exPrefix <$> prefixOperator n <*> namesSfx <*> nList n (sW*>x)

--DONE:left-currying can be done as $a [ `b, `c ]

expr,curried::Syntax f => f Expr
curried = exprCurriable True
expr = exprCurriable False
exprCurriable::Syntax f => Bool -> f Expr
exprCurriable curriedLeft = e 0 
	where
	e::Syntax f => Int -> f Expr
	e i = [
		let 
			r 0 = infixOperator 0 <*> namesSfx <☆> nList 1 x 
			r 1 = infixOperator 1 <*> namesSfx <*> (cons <$> (sW*>self') <*> nList 1 (sW*>x))
			-- r j = infixOperator j <*> namesSfx <*> (cons <$> (sW*>self') <*> nList j (sW*>x)) 
				--TODO:only the right most expr should be x
		
			infixRest = (r 0 <|> r 1)
			lrRest
				=		lrrInfix <$> infixRest
				<|>	lrrPostfix <$> (alts $ map (postfixNRest self') arities)
				<|>	lrrGrouping <$> sW *> text "!" *> namesSfx
			(self', leftmost) = if curriedLeft 
				-- then exLeftRec <$> (exLeftmostPlaceholder <$> pure ()) <*> infixRest
				then (expr,) $ exCurriedLeft <$> (infixRest <|> postfixOperator 1<*>namesSfx <*>pure [])
				else (self,x)
		in foldl exLeftRec <$> leftmost <*> manySfx lrRest
		,
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

snippet = between sW sW expr

test n p p' f x = do
	putStrLn "========================================="
	putStrLn x
	let a = take n $ Parser.parse p x
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
	nCheck<-getArgs
	let _t = test (head $ map read nCheck++[1]) snippet snippet $ putStrLn.groom
	--let _t0 = test 1 snippet snippet $ \_->return ()
	let t = test 999 expr expr print
	let t' = test 999 expr expr $ putStrLn.groom
	let t0 = test 999 expr expr $ \_->return ()
	let z x s = putStr ">>" >> print s >> mapM_ print (Parser.parse x s)
	-- test 999 curriedList curriedList (putStrLn.groom) "(`html@1,`text@2)"
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

	t' "$`[_-$a@x,_+$b@y,_`find`$c@z,_+$+//@+//@]"

	tf "sampletests"
	-- tf "sampletests1"

	putStrLn "*******\nDONE!!!\n*******"
	return ()
