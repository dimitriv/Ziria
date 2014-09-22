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
  | "times" <expr> <comp>
  | "for" <var-bind> "in" "[" <interval> "]" <comp>

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
  | "fun" <comp-ann> <var-bind> <comp-params> "{" <decl>* <commands> "}"
  | "fun" <var-bind> <params> "{" <decl>* <stmts> "}"
  | "let" <comp-ann> <var-bind> "=" <comp>
  | "let" <var-bind> "=" <expr>

<struct> ::= "struct" IDENT "=" "{" (IDENT ":" <base-type>)*";" "}"
<params> ::= "(" (IDENT ":" <base-type>)*"," ")"
<comp-params> ::= "(" (IDENT ":" (<base-type> | <comp-base-type>))*"," ")"
<comp-base-type> ::= "ST" ("T" | "C" <base-type>) <base-type> <base-type>
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

## Annotations

```
<type-ann> ::= "[" <base-type> "]"
<vect-ann> ::= "<="? "!"? <range>
<range>    ::= "[" <int> "," <int> "]"
<comp-ann> ::= "comp" <range>?
```

## The expression language

### Types

The language of types is given by `<basetype>` that describe expression types.
`int` and `complex` default to `int32` and `complex32` unless type inference
determines otherwise.

```
<basetype>
  ::= bit | bool | double 
    | int | int8 | int16 | int32
    | complex | complex16 | complex32
    | struct TYPENAME
    | arr <basetype>
    | arr[INTEGER] <basetype>
    | arr[length(VARNAME)] <basetype>
```

Arrays can have either:

1. A programmer-unspecified length, that type inference is going to try to infer.
2. They can have a `INTEGER` literal length. As a convenience, the parser
   actually does support also simple expressions that can statically be
   evaluated to integer literals such as `1024 + 48`.
3. Arrays can specify fo have the length of another array bound earler, with
   `arr[length(VARNAME)] <basetype>`.

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

First of all some operator definitions. Unary operators are:

```
-- Unary operators
<unop> ::= 
    -
  | not
  | ~
  | length
```

Binary operators:

```
-- Binary operators
<binop> ::= 
    **       -- exponentiation
  | *        -- multiplication
  | /        -- division
  | %        -- modulo

  | +        
  | - 

  | <<       -- shift operators
  | >>

  | <        -- comparisons
  | <=
  | >
  | >=

  | &        -- bitwise and
  | ^        -- bitwise xor
  | |        -- bitwise or
  | &&       -- logical and
  | ||       -- logical or
```

The binary operators are presented in several groups, highest-priority
group first, and within the same group highest-priority operator
first. They are what one would expect in a C or Pascal-like language. 

The actual syntax of expression follows: 
```
<expr> ::= 
    <expr> binop <expr>
  | unop <expr>
  | (<expr>)
  | <term>
```

And terms are:

```
<term> ::= 
    <value>

    -- structure initialization
  | TYPENAME { FLDNAME1 = <expr>; ... FLDNAMEn = <expr> }
  
    -- dereferencing expressions
  | <dexpr>
  
    -- casts 
  | <basetype>(<expr>)
  
    -- function calls
  | FUNCNAME(<eargs>)
  
  | -- conditionals
    if <expr> then <expr> else <expr>
  
  | -- let definitions
  let <varbind> = <expr> in <expr>

-- expression arguments 
<eargs> ::= null | <expr>(,<expr>)*

<varbind> ::= VARNAME | ( VARNAME : <basetype>) 

<dexpr> ::=
    -- variable
    VARNAME

    -- field projection
  | <dexpr>.FLDNAME
  
    -- subarray derefencing
  | <dexpr>[INTEGER:INTEGER]

    -- array element dereferencing
  | <dexpr>[INTEGER]

<value> ::= ()                -- unit
      | true | false      -- boolean values
      | '0 | '1           -- bit values
        | STRING            -- e.g. "blink"
      | INTEGER           -- e.g. 1,2,5,6 ...
      | FLOAT             -- e.g. 3.14
      | { <values> }      -- array values

<values> ::= null | <value>(,<value>)* 
```

Now, in addition to the expressions and terms, Blink provides support for "imperative" blocks of statements.

```
<stmts> ::= null | <stmt>(;<stmt>)*?;
<stmtblock> ::= <stmt> | { <stmts> }
<stmt> ::=
    -- assignment
    VARNAME := <expr>
  
    -- return
  | return <expr>

    -- function call
  | FUNCNAME(<eargs>)

    -- for loops
  | for VARNAME in <range> <stmtblock>
  
    -- while loops
  | while <expr> <stmtblock>

    -- conditional commands
  | if <expr> then <stmtblock> ?(else <stmtblock>)

    -- let bindings
  | let <varbind> = <expr> ?(in <stmt>)

    -- print to stdout
  | print <exprs>
  | println <exprs>
  
    -- exit current thread with an error message
  | error STRING

<exprs> ::= <expr>(,<expr>)*

<range> ::=
    -- (inclusive) interval range
    [INTEGER:INTEGER]

    -- start and length based range
  | [<expr>,<expr>]
  
```

A caveat: currently we dont support parsing of nested array values.
