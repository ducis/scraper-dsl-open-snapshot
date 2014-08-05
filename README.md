scraper-dsl
===========

dsl targeted for web scraping which compiles to phantom.js snippets

Previously I found peggy behaving differently when compiled and interpreted. So just cp or symlink the .hs to somewhere under your PATH.

Elements
	Selector
	Traversing function abbreviation (maybe leave some untouched?)
		arbitrary fixity for fullname/abbreviation/symbol
		infix `some`
		postfix `some
		prefix some`
		what about arity?
		υ		add
				addBack
				children
		←		closest
				contents
				end
				eq
				filter
		→		find
				first
				has
				last
		↓		next
				nextAll
				nextUntil
				not
				offsetParent
				parent
				parents
				parentsUntil
		↑		prev
				prevUntil
				siblings
				slice

	Naming
	Branching
	Multiplicity
	Functional map/filter ? regex catch and replace?

How to deal with name scoping?
	Separate selector structure and namespace nesting
How to structure output?
	{a b} * {d e f}
	a * d
	a * e
	a * f
	b * d
	b * e
	b * f


Explicit syntax tree (the long form)
	invertible-syntax
	partial-isomorphisms

commandline utility integratable into dwb
