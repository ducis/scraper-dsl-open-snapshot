{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}
module DSL.Scrapoo.ParseTree where

type Quote = String
data Name = Name Char (Maybe String) String (Maybe String)
	deriving (Eq,Read,Show,Ord)
data Operator 
	= OpSymbolic String 
	| OpAlphabetic String --Including abbreviation
	| OpComposed Char Char [Expr]
	deriving (Eq,Read,Show,Ord)
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExSlot
	| ExBlock Char Char [Expr]
	| ExLeftRec Expr LeftRecRest
	| ExCurriedLeft Operator [Name] [Expr]
	| ExPrefix Operator [Name] [Expr]
	| ExNamed Expr Name
	deriving (Eq,Read,Show,Ord)
data LeftRecRest
	= LrrInfix Operator [Name] [Expr]
	| LrrPostfix [Expr] Operator [Name]
	| LrrGrouping [Name] --essentially a unary postfix operator without arity mark 
		--TODO: when applied to the leftmost 'atom', breaks current block.
		--		$a-[$b!-$c,$d!-$e]
		--		becomes
		--		[$a-$b-$c,$a-$d-$e]
		--		Or should it be done at typing? Like coercing 'leftmost' string literal to element set
	deriving (Eq,Read,Show,Ord)


