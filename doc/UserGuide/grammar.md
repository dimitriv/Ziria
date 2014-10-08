# Ziria grammar definition

We give here rules for the formal syntax of Blink. Comments in Blink are given
with `--` for single-line comments and `{- ... -}` for multi-line comments.
Some notation for our informal BNF syntax below:

- `<non-terminals>` are written in angular brackets
- Literal string (tokens) as written as strings: `"repeat"`
- `<foo>*` denotes an arbitrary number of `<foo>`s
- `<foo>*","` denotes an arbitrary number of `<foo>`s separated by `","`
- `<foo>?` denotes an optional <foo>
- `null` denotes the empty production (just whitespace).
- We use capitals to denote several syntactic classes:
  * `VARNAME` : identifiers (that is, consisting of alphanumeric characters and "_")
  * `FLDNAME` : identifiers for field names
  * `FUNCNAME`: identifiers for expression or computation functions
  * `TYPENAME`: identifiers for named struct types
  * `INTEGER` : integer literals like 1,2,3
  * `STRING`  : string literals like "foobar\n"
  * `FLOAT`   : floating point literals like 3.14

## The computation language

### The main program

```
<program> ::= <decls> <comp>
<decls>   ::= (<decl>;)*
```

`<decl>` comes from the expression language.

### Computation expressions

```
<comp> ::=
    "standalone" <comp>
  | "repeat" <vect-ann>? <comp>
  | "until" <expr> <comp>
  | "while" <expr> <comp>
  | ("unroll" | "nounroll")? "times" <expr> <comp>
  | ("unroll" | "nounroll")? "for" <var-bind> "in" "[" <interval> "]" <comp>

  | <comp> ">>>" <comp>
  | <comp> "|>>>|" <comp>

  | <term>
```

where

* the prefix operators `standalone`, `repeat`, `until`, `while`, `times` and
  `for` all have the same precedence, and all bind stronger than the infix
  operators `>>>` and `|>>>|`
* `>>>` and `|>>>|` are both left associative
* `>>>` binds stronger than `|>>>|`

A term is defined as

```
term ::=
    "(" <comp> ")"
  | "return" <expr>"
  | "emit" <expr>
  | "emits" <expr>
  | "takes" <expr>
  | "filter" <expr>
  | "read" <type-ann>?
  | "write" <type-ann>?
  | "map" <vect-ann>? <var-bind>
  | "take"
  | IDENT
  | IDENT "(" <expr>*"," ")"
  | "if" <expr> "then" <comp> "else" <comp>
  | <let-decl> "in" <comp>
  | "do" <stmt-block>
  | "seq"? "{" <commands> "}"
```  

where `<stmt_block>` comes from the expression language.

NOTE: The syntax for calling unknown functions is 

```
IDENT "(" <expr>*"," ")"
```

as in the grammar. For calling known functions however we expect a
comma-separated list of as many arguments as the function expect, which can
either be computations `<comp>` or expressions `<expr>`.

## Declarations

```
<let-decl> ::=
  | <decl>
  | <struct>
  | "let" "external" IDENT <params> ":" <base-type>
  | "fun" <comp-ann> <cvar-bind> <comp-params> "{" <decl>* <commands> "}"
  | "fun" <var-bind> <params> "{" <decl>* <stmts> "}"
  | "let" <comp-ann> <cvar-bind> "=" <comp>
  | "let" <var-bind> "=" <expr>

<struct> ::= "struct" IDENT "=" "{" (IDENT ":" <base-type>)*";" "}"
<params> ::= "(" (IDENT ":" <base-type>)*"," ")"
<comp-params> ::= "(" (IDENT ":" (<base-type> | <comp-base-type>))*"," ")"
<comp-base-type> ::= "ST" ("T" | "C" <base-type>) <base-type> <base-type>
<cvar-bind> ::= IDENT | "(" IDENT ":" <comp-base-type> ")"
```

(`<base-type>` comes from the expression language.)

## Commands

```
<commands> ::= (<command> ";")*

<command> ::=
    <let-decl>
  | "if" <expr> "then" <comp>
  | <var-bind> "<-" <comp>
  | <comp>
```

The last command in the list must be a computation.

## Annotations

```
<type-ann> ::= "[" <base-type> "]"
<vect-ann> ::= "<="? "!"? <range>
<range>    ::= "[" <int> "," <int> "]"
<comp-ann> ::= "comp" <range>?
```

## The expression language

### Types

The language of types is given by `<base-type>` that describe expression types.
`int` and `complex` default to `int32` and `complex32` unless type inference
determines otherwise.

```
<base-type> ::=
    "()"
  | "bit"
  | "int"
  | "int8"
  | "int16"
  | "int32"
  | "int64"
  | "double"
  | "bool"
  | "complex"
  | "complex8"
  | "complex16"
  | "complex32"
  | "complex64"
  | "struct" IDENT
  | "arr" "[" "length" IDENT "]" <base-type>
  | "arr" "[" <expr> "]" <base-type>
  | "arr" <base-type> -- length inferred from context
  | "(" <base-type> ")"
```  

Note that "int" means "int32".  Bit widths in the source language are _always_
given, and we don't support bitwidth polymorphism (unknown bit widths are only
used in the type checker for the types of literals).

Arrays can have either:

1. A programmer-unspecified length, that type inference is going to try to infer.
2. They can have a `INTEGER` literal length. As a convenience, the parser
   actually does support also simple expressions that can statically be
   evaluated to integer literals such as `1024 + 48`.
3. Arrays can specify fo have the length of another array bound earler, with
   `arr[length(VARNAME)] <base-type>`.

Hence you may well have:
```
let f(x : arr[32] int) =
  var y : arr int;              -- length inferred
  var z : arr[length(x)] int;   -- same as length of x
  y := x;                       -- fixes length of y to 32
  z := y;
  ...
in ... 
```

### Expressions

The actual syntax of expression is: 

```
<expr> ::=
    "-"      <expr>     -- negation
  | "not"    <expr>     -- not
  | "~"      <expr>     -- bitwise negation

  | "length" <expr>     -- length

  | <expr> "**" <expr>  -- exponentiation
  | <expr> "*"  <expr>  -- multiplication
  | <expr> "/"  <expr>  -- division
  | <expr> "%"  <expr>  -- remainder

  | <expr> "+"  <expr>  -- addition
  | <expr> "-"  <expr>  -- subtraction

  | <expr> "<<" <expr>  -- shift left
  | <expr> ">>" <expr>  -- shift right

  | <expr> "<"  <expr>  -- less than
  | <expr> "<=" <expr>  -- less than or equal to
  | <expr> ">"  <expr>  -- greater than
  | <expr> ">=" <expr>  -- greater than or equal to

  | <expr> "&"  <expr>  -- bitwise AND

  | <expr> "^"  <expr>  -- bitwise XOR

  | <expr> "|"  <expr>  -- bitwise OR

  | <expr> "==" <expr>  -- equality
  | <expr> "!=" <expr>  -- inequality

  | <expr> "&&" <expr>  -- logical AND
  | <expr> "||" <expr>  -- logical OR
  | <term>
```  

where we have grouped operators with the same precedence, and groups of
operators listed earlier have higher precedence.


And terms are:

```
<term> ::=
    "(" <expr> ")"
  | "()"
  | <value>
  | TYPENAME "{" (FLDNAME "=" <expr>)*";")     -- struct init
  | IDENT ("." IDENT | "[" <range> "]")*  -- struct or array index
  | IDENT "(" <expr>*"," ")"              -- function call or cast
  | IDENT                                 -- variable
  | "let" <var-bind> "=" <expr> "in" <expr>
  | <decl> "in" <expr>
  | "if" <expr> "then" <expr> "else" <expr>

<var-bind> ::= IDENT | "(" IDENT ":" <base-type> ")"
<range>    ::= <interval> | <expr>
<interval> ::= <expr> ":" <expr> | <expr> "," <expr>
<value>    ::= <scalar-value> | "{" <scalar-value>*"," "}"
<decl>     ::= "var" IDENT ":" <base-type> (":=" <expr>)?

<scalar-value> ::=
    "(" <scalar-value> ")"
  | "true"
  | "false"
  | "'0"
  | "'1"
  | "()"
  | FLOAT
  | STRING
  | INT
```

Now, in addition to the expressions and terms, Blink provides support for "imperative" blocks of statements.

```
<stmt-block> ::= "{" <stmts> "}" | <stmt>
<stmts> ::= <stmt>*";"

<stmt> ::=
    "let" <var-bind> "=" <expr> ("in" <stmt>)?
  | <decl> ("in" <stmt>)?
  | ("unroll" | "nounroll")? "for" <var-bind> "in" "[" <interval> "]" <stmt-block>
  | "while" "(" <expr> ")" <stmt-block>
  | "if" <expr> "then" <stmt-block> ("else" <stmt-block>)?
  | "return" <expr>
  | "print" <expr>*","
  | "println" <expr>*","
  | "error" STRING
  | FUNCNAME "(" <expr>*"," ")"
  | VARNAME ("." IDENT)* ("[" <range> "]")? ":=" <expr>
```

A caveat: currently we dont support parsing of nested array values (TODO: is
that still true?).
