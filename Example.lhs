% An invertible syntax description of a small example language
% Tillmann Rendel
% April to November, 2010

Introduction
============

In this example file, the packages `partial-isomorphisms` and
`invertible-syntax` are used to describe the syntax of a small
language. This syntax descriptions can be used for both parsing
and printing. An earlier version of this document was published
as part of

  * Tillmann Rendel and Klaus Ostermann. 
    Invertible syntax descriptions: 
    Unifying parsing and pretty printing. 
    Haskell symposium, 2010.
    
    http://www.informatik.uni-marburg.de/~rendel/unparse/

Abstract Syntax
===============

The abstract syntax of the example language is encoded with
abstract data types.

%if False

> {-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
> module Example where
>
> import Prelude (Show (..), Read (..), Eq (..), String, Integer,
>                 map, (++), Maybe (..), ($), fst, not, elem, 
>                 notElem, reads, Char)
> 
> import Control.Category (id, (.))
>
> import Control.Monad (mplus)
>
> import Data.Char (isLetter, isDigit)
>
> import qualified Text.ParserCombinators.Parsec as Parsec
>
> import Control.Isomorphism.Partial
> import Control.Isomorphism.Partial.TH
> import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
> import Text.Syntax

%endif

> data Expression
>     =  Variable String
>     |  Literal Integer
>     |  BinOp Expression Operator Expression
>     |  IfZero Expression Expression Expression
>   deriving (Show, Eq)
>
> data Operator
>     =  AddOp 
>     |  MulOp 
>   deriving (Show, Eq)

The Template Haskell macro `defineIsomorphisms` is used to
generate partial isomorphisms for the data constructors.

> $(defineIsomorphisms ''Expression)
> $(defineIsomorphisms ''Operator)

Syntax descriptions
-------------------

The first character of an identifier is a letter, the remaining
characters are letters or digits. Keywords are excluded.

> keywords = ["ifzero", "else"]

> letter, digit :: Syntax delta => delta Char
> letter  =  subset isLetter <$> token
> digit   =  subset isDigit <$> token

> identifier 
>   = subset (`notElem` keywords) . cons <$> 
>       letter <*> many (letter <|> digit)

Keywords are literal texts but not identifiers.

> keyword :: Syntax delta => String -> delta ()
> keyword s = inverse right <$> (identifier <+> text s)

Integer literals are sequences of digits, processed by read
resp. show. 

> integer :: Syntax delta => delta Integer
> integer = Iso read' show' <$> many digit where
>   read' s  =  case [ x | (x, "") <- reads s ] of
>                 [] -> Nothing
>                 (x : _) -> Just x
>               
>   show' x  =  Just (show x)

A parenthesized expressions is an expression between parentheses.

> parens = between (text "(") (text ")")

The syntax descriptions `ops` handles operators of arbitrary 
priorities. The priorities are handled further below. 

> ops  =    mulOp  <$>  text "*"
>      <|>  addOp  <$>  text "+"

We allow optional spaces around operators.

> spacedOps = between optSpace optSpace ops

The priorities of the operators are defined in this function. 

> priority :: Operator -> Integer
> priority  MulOp  =  1
> priority  AddOp  =  2

Finally, we can define the `expression` syntax description. 

> expression = exp 2 where

>   exp 0  =    literal    <$>  integer
>          <|>  variable   <$>  identifier
>          <|>  ifZero     <$>  ifzero
>          <|>  parens (skipSpace *> expression <* skipSpace)
>   exp 1  =    chainl1  (exp 0)  spacedOps  (binOpPrio 1)
>   exp 2  =    chainl1  (exp 1)  spacedOps  (binOpPrio 2)

>   ifzero  =    keyword "ifzero" 
>           *>   optSpace  *>  parens (expression)
>           <*>  optSpace  *>  parens (expression) 
>           <*>  optSpace  *>  keyword "else"  
>           *>   optSpace  *>  parens (expression)
>   
>   binOpPrio n 
>     = binOp . subset (\(x, (op, y)) -> priority op == n)

This syntax description is correctly processing binary operators
according to their priority during both parsing and printing.
Similar to the standard idiom for expression grammars with infix
operators, the description of `expression` is layered into
several `exp i` descriptions, one for each priority level. The
syntax description combinator `chainl1` parses a left-recursive
tree of expressions, separated by infix operators. Note that the
syntax descriptions `exp 1` to `exp 2` both use the same syntax
descriptions `ops` which describes all operators, not just the
operators of a specific priority. Instead, the correct operators
are selected by the `binOpPrio n` partial isomorphisms. The
partial isomorphism `binOpPrio n` is a subrelation of
`binOp` which only accepts operators of the priority level `n`.

While parsing a high-priority expressions, the partial
isomorphism will reject low-priority operators, so that the
parser stops processing the high-priority subexpression and
backtracks to continue a surrounding lower-priority expression.
When the parser encounters a set of parentheses, it allows
low-priority expressions again inside.

Similarly, during printing a high-priority expression,
the partial isomorphism will reject low-priority operators,
so that the printer continues to the description of `exp 0`
and inserts a matching set of parentheses.

All taken together, the partial isomorphisms `binOpPrio n` not
only control the processing of operator priorities for both
printing and parsing, but also ensure that parentheses are
printed exactly where they are needed so that the printer output
can be correctly parsed again. This way, correct round trip
behavior is automatically guaranteed. 

The following evaluation shows that operator priorities are
respected while parsing.

    > parse expression "ifzero (2+3*4) (5) else (6)"
    [IfZero  (BinOp (Literal 2) AddOp 
               (BinOp (Literal 3) MulOp (Literal 4))) 
             (Literal 5) 
             (Literal 6)]

And this evaluation shows that needed parentheses are inserted
during printing.
            
    >  print expression 
         (BinOp  (BinOp  (Literal 7) AddOp 
                         (Literal 8)) MulOp 
                 (Literal 9))
    Just "(7 + 8) * 9"

By implementing whitespace handling and associativity and
priorities for infix operators, we have shown how to implement
two non-trivial aspects of syntax descriptions which occur in
existing parsers and pretty printers for formal languages. We
have shown how to implement well-known combinators like `between`
and `chainl1` in our framework, which enabled us to write the
syntax descriptions in a style which closely resembles how one
can program with monadic or applicative parser combinator
libraries.
