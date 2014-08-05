{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
module Example where
import Prelude (Show (..), Read (..), Eq (..), String, Integer,
                map, (++), Maybe (..), ($), fst, not, elem, 
                notElem, reads, Char, return, print, putStrLn,unlines)

import Control.Category (id, (.))
import Control.Monad (mplus)
import Data.Char (isLetter, isDigit)
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Text.Syntax
import qualified Text.Syntax.Parser.Naive as Parser
import qualified Text.Syntax.Printer.Naive as Printer
import Data.Maybe
data Expression
    =  Variable String
    |  Literal Integer
    |  BinOp Expression Operator Expression
    |  IfZero Expression Expression Expression
  deriving (Show, Eq)
data Operator
    =  AddOp 
    |  MulOp 
  deriving (Show, Eq)
$(defineIsomorphisms ''Expression)
$(defineIsomorphisms ''Operator)
keywords = ["ifzero", "else"]
letter, digit :: Syntax delta => delta Char
letter  =  subset isLetter <$> token
digit   =  subset isDigit <$> token
identifier 
  = subset (`notElem` keywords) . cons <$> 
      letter <*> many (letter <|> digit)
keyword :: Syntax delta => String -> delta ()
keyword s = inverse right <$> (identifier <+> text s)
integer :: Syntax delta => delta Integer
integer = Iso read' show' <$> many digit where
  read' s  =  case [ x | (x, "") <- reads s ] of
                [] -> Nothing
                (x : _) -> Just x
              
  show' x  =  Just (show x)
parens = between (text "(") (text ")")
ops  =    mulOp  <$>  text "*"
     <|>  addOp  <$>  text "+"
spacedOps = between optSpace optSpace ops
priority :: Operator -> Integer
priority  MulOp  =  1
priority  AddOp  =  2
expression = exp 2 where
  exp 0  =    literal    <$>  integer
         <|>  variable   <$>  identifier
         <|>  ifZero     <$>  ifzero
         <|>  parens (skipSpace *> expression <* skipSpace)
  exp 1  =    chainl1  (exp 0)  spacedOps  (binOpPrio 1)
  exp 2  =    chainl1  (exp 1)  spacedOps  (binOpPrio 2)
  ifzero  =    keyword "ifzero" 
          *>   optSpace  *>  parens (expression)
          <*>  optSpace  *>  parens (expression) 
          <*>  optSpace  *>  keyword "else"  
          *>   optSpace  *>  parens (expression)
  
  binOpPrio n 
    = binOp . subset (\(x, (op, y)) -> priority op == n)

main = do
		--let b = map (Printer.print p) a
		--print b
		--print b
	let p = expression
	let x = "ifzero (2+3*4) (5) else (6)"
	let a = Parser.parse p x
	print a
	let b = map (Printer.print p) a
	print b
	let c = catMaybes b
	putStrLn $ unlines c
	--putStrLn $ catMaybes b
	return ()
