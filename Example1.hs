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
    =  Literal Integer
    |  BinOp Expression Operator Expression
  deriving (Show, Eq)
data Operator
    =  AddOp 
  deriving (Show, Eq)
$(defineIsomorphisms ''Expression)
$(defineIsomorphisms ''Operator)
digit :: Syntax delta => delta Char
digit   =  subset isDigit <$> token
integer :: Syntax delta => delta Integer
integer = Iso read' show' <$> many digit where
  read' s  =  case [ x | (x, "") <- reads s ] of
                [] -> Nothing
                (x : _) -> Just x
              
  show' x  =  Just (show x)
parens = between (text "(") (text ")")
ops  = addOp  <$>  text "+"
spacedOps = between optSpace optSpace ops
expression = chainl1 (exp0) spacedOps binOp
  
exp0  =    literal    <$>  integer
		<|>  parens (skipSpace *> expression <* skipSpace)
  
main = do
		--let b = map (Printer.print p) a
		--print b
		--print b
	let p = expression
	--let x = "ifzero (2+3*4) (5) else (6)"
	let x = "(1+2)+(3+4)+5+6"
	putStrLn x
	let a = Parser.parse p x
	print a
	let b = map (Printer.print p) a
	print b
	let c = catMaybes b
	putStrLn $ unlines c
	--putStrLn $ catMaybes b
	return ()
