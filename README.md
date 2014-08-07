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
					selector::Selector
					elements::Element+
					html::String
					selection::jQuery
					selector, context::Element
				addBack
					selector?
				children
					selector?
		←		closest
					selector
					selector, context?
					selection
					element
					(deprecated) selectors::Selector+, context?
				contents
					()
				end
					()
				eq
					index::int
				filter
					selector
					function
					elements
					selection
		→		find
					selector
					selection
					element::Element
				first
					()
				has
					selector
					element
				is :: -> Boolean
					selector
					function(index,element) :: -> Boolean
					selection
					elements
				last
					()
				map :: -> jQuery
					function(index,element) :: -> anything
		↓		next 
					selector?
				nextAll
					selector?
				nextUntil
					selector?, filter?
					(element::Element|jQuery)?, filter?
				not
					selector::Selector|Element+
					function
					selection
				offsetParent
					()
				parent
					selector?
				parents
					selector?
				parentsUntil
					selector?, filter?
					(element::Element|jQuery)?, filter?
		↑		prev
					selector?
				prevAll
					selector?
				prevUntil
					selector?, (filter::Selector)?
					element::Element|jQuery, filter?
				siblings
					selector?
				slice
					start::Int, end?

	Naming
	Branching
	Multiplicity
	Functional map/filter ? regex catch and replace?

'filter' arguments are removed

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

transformation

manipulation
