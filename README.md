## Delimited Tuple Markup Language

A minimalist all-purpose markup language: nested lists of whitespace friendly text.

### Why DTML?

DTML allows you to express human-readable data with a syntax much simpler and less cluttered than other all-purpose markup/data languages. DTML reserves only five symbols, yet is flexible enough to express an extremely wide range of data semantics.

In contrast, many software engineers find XML to be overly verbose, irregular, complex, and bulky for most tasks. Related fact: The book "XML in a Nutshell" is **714** pages long.

* _"XML sometimes feels an awful lot like using an enormous sledgehammer to drive common household nails." - [Jeff Atwood](http://www.codinghorror.com/blog/2008/05/xml-the-angle-bracket-tax.html)_


### What is in this repository?

This repository contains a DTML Parser and Document Tree API implemented in Rust.

## DTML Examples

### DTML example demonstrating markup semantics:

```
[html|[
    Hello. This is an example | [b|language] | test. |
    [div| [class|testc] |[ And this text is enclosed in a div. ]] |
    [a| [href|google.com] |[ Click this link | [i|now] ]]
]]
```

Compare to HTML/XML:
```
<html>
	Hello. This is an example <b>language</b> test.
	<div class='testc'> And this text is enclosed in a div. </div>
	<a href='google.com'> Click this link <i>now</i> </a>
</html>
```


### DTML example demonstrating key-value pair semantics:

```
[
    [first name| [John] ]|
    [last name| [Smith] ]|
    [age| [25] ]|
    [address|[
        [street address| [21 2nd Street] ]|
        [city| [New York] ]|
        [state| [NY] ]|
        [postalCode| [10021] ]|
    ]]|
]
```

Compare to JSON:
```
{
    "first name": "John",
    "last name": "Smith",
    "age": 25,
    "address": {
        "street address": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
    }
}
```


### DTML example describing a 3D pyramid object for OpenGL:

This example shows how one might use DTML to load/store 3D models for an OpenGL graphics application. In addition to natural key-value pair semantics, DTML enables concise lists of vertex coordinates and element indexes, making this very natural to read, write, and organize.

```
[opengl model| [
    [mode|indexed triangles] |

    [buffer| [vertex] | [
        [attrib| [position] | [layout|interleaved] | [type|float3] ] |
        [attrib| [uv] | [layout|interleaved] | [type|float2] ] |

        [data| [position] |[ [0 1 0] | [-1 0 -1] | [1 0 -1] | [-1 0 1] | [1 0 1] ]] |
        [data| [uv] |[ [0.5 0.0] | [0 1] | [1 1] | [0 1] | [1 1] ]] |
    ]] |

    [buffer| [element16] | [
        [data| [ [0]|1|2|  [0]|2|3|  [0]|3|4|  [0]|4|1| ]] |
    ]] |

    [texture| [0] | [
        [file|media/rock.png] |
        [filter min|nearest] |
        [filter mag|bilinear] |
    ]] |

    [program| [fragment] | [media/rocky.frag]] |
    [program| [vertex] | [media/rocky.vert]] |
]]
```

---

## DTML Syntax

### Basic Tuples

The most basic tuple is a monad (1-tuple) with a value of empty string:
```

```

But that's pretty boring, let's add some text. Here is a monad with the value `Hello, World!`:
```
Hello, World!
```

Simple, now let's split it into a pair (2-tuple) with values equal to `Hello, ` and `World!`:
```
[Hello, |World!]
```

### Whitespace Basics

Note that the whitespace is captured into the first element of the pair. If we want to avoid this, we can utilize the `[` and `]` characters as parenthesis to capture only what we want:
```
[[Hello,] |World!]
```

Whitespace is captured it two case: when it's adjacent to text, and when it's the only thing present inside "parenthesis". With this information, we can see that the following tuples are all equivalent:
```
[Hello, |World!]
```

```
[[Hello, ] |World!]
```

```
[[Hello, ]      |World!]
```

```
[[[Hello, ]      ]|[ [ [World!] ] ] ]]
```

```
[[[Hello,]      ][ ]|[ [ [World!] ] ] ]]
```

### Escape Sequences

If you want to encode one of the reserved characters, you can use an escape sequence.

```
\[ \] \| \# \\ \n \r \t \x8f \u[003A]
```

### Empty & Special Tuples

It also possible to create an empty tuple (0-tuple) by place an `[` and `]` with nothing in between (excluding comments):

```
[]
```

Currently the only special tuple is the Null tuple:

```
\0
```

---

## DTML Specification

### Notation

The syntax is specified using [Extended Backus-Naur Form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) using the [ISO/IEC 14977](http://standards.iso.org/ittf/PubliclyAvailableStandards/s026153_ISO_IEC_14977_1996\%28E%29.zip) standard syntax.

#### Character Sets

To start, let's define our character sets: Control Characters, Comment Characters, Whitespace Characters, Text Characters, Special Characters.

##### Control Characters

Control Characters are the characters reserved for use by the syntax and are used to define the document structure. They are `[`, `]`, `|`, `\`, and `#`.

|                   |   |                                                                                                       |
|-------------------|---|-------------------------------------------------------------------------------------------------------|
| Control Character | = | Open Token &#124; Close Token &#124; Divider Token &#124; Escape Character &#124; Comment Character ; |
| Open Token        | = | "[" ;                                                                                                 |
| Close Token       | = | "]" ;                                                                                                 |
| Divider Token     | = | "&#124;" ;                                                                                            |
| Escape Character  | = | "\\" ;                                                                                                |
| Comment Character | = | "#" ;                                                                                                 |

##### Comment Character
Comment Characters are used by the syntax of comments.

|                                 |   |                                                                           |
|---------------------------------|---|---------------------------------------------------------------------------|
| Line Feed Character             | = | ? Unicode code point U+000A ? ;                                           |
| Line Comment Character          | = | ? a Unicode code point ? - Line Feed Character ;                          |
| Line Comment Starting Character | = | Line Comment Character - Open Token ;                                     |
| Block Comment Character         | = | ? a Unicode code point ? - Comment Character - Open Token - Close Token ; |

##### Whitespace Characters

Whitespace Characters are characters which can be used in some circumstances to make the document more readable without changing it's meaning. They are defined by the [Unicode WSpace property](http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:whitespace:]).

|                      |   |                                                             |
|----------------------|---|-------------------------------------------------------------|
| Whitespace Character | = | ? a Unicode code point with character property WSpace=Y ? ; |

##### Text Characters

Text Characters are the characters allowed for document content. The set of Text Characters includes every Unicode character except those which are Control Characters or Whitespace Characters.

Hex characters are characters used to encode hexadecimal values for escape sequences.

|                |   |                                                                                                                                                                                                                                              |
|----------------|---|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Text Character | = | ? a Unicode code point ? - Control Character - Whitespace Character ;                                                                                                                                                                        |
| Hex Character  | = | "0" &#124; "1" &#124; "2" &#124; "3" &#124; "4" &#124; "5" &#124; "6" &#124; "7" &#124; "8" &#124; "9" &#124; "a" &#124; "b" &#124; "c" &#124; "d" &#124; "e" &#124; "f" &#124; "A" &#124; "B" &#124; "C" &#124; "D" &#124; "E" &#124; "F" ; |

##### Special Characters

Special Characters are characters which are used to create Monads of special meaning. The only special character at present is `0`. `0` is meant to signify a null value.

|                   |   |        |
|-------------------|---|--------|
| Special Character | = | Null ; |
| Null              | = | "0" ;  |

#### Token

Using the character sets we defined, we next define some Tokens: Comment Token, Escape Token, Whitespace Token, Text Token, and Special Token.

##### Comment Token

Comment Tokens come in two forms, Line Comments and Block Comments. Comments do not have a value and are ignored.

|               |   |                                     |
|---------------|---|-------------------------------------|
| Comment Token | = | Line Comment &#124; Block Comment ; |

Line Comments begin with a Comment Character followed by anything but an Open Token. Line Comments end with a Line Feed Character.

|              |   |                                                                                                           |
|--------------|---|-----------------------------------------------------------------------------------------------------------|
| Line Comment | = | Comment Character, [ Line Comment Starting Character, { Line Comment Character } ], Line Feed Character ; |

Block Comments begin with a Comment Character followed by an Open Token. Block Comments end with a Close Token Followed by a Comment Character. Block Comments can be nested.

|                       |   |                                                                                                                            |
|-----------------------|---|----------------------------------------------------------------------------------------------------------------------------|
| Block Comment         | = | Comment Character, Open Token, Block Comment Content, Close Token, Comment Character ;                                     |
| Block Comment Content | = | [ Block Comment ], [ Block Comment Text ], { Block Comment, [ Block Comment Text ] } ;                                     |
| Block Comment Text    | = | Block Comment Helper &#124; Block Comment Closed &#124; Block Comment Open ;                                               |
| Block Comment Open    | = | Comment Character, [ Block Comment Helper &#124; Block Comment Open ] ;                                                    |
| Block Comment Closed  | = | Open Token, [ Block Comment Text ] ;                                                                                       |
| Block Comment Helper  | = | Block Comment Character, [ Block Comment Text ] &#124; Close Token, [ Block Comment Helper &#124; Block Comment Closed ] ; |

##### Escape Token

Escape Tokens are a way of encoding any Unicode code point within a document. Escape Tokens begin with an Escape Character. If this is followed by:
* A Control Character, the value of the Escape Token is the Control Character.
* An "n", the value is U+000A.
* An "r", the value is U+000D.
* A "t" the value is U+0009.
* A Hex Sequence, the value is a single code point corresponding to the 8-bit character code represented by the Hex Characters.
* A Unicode Sequence, the value is a single code point corresponding to the 24-bit Unicode character code represented by the Hex Character(s).

|                  |   |                                                                                                                        |
|------------------|---|------------------------------------------------------------------------------------------------------------------------|
| Escape Token     | = | Escape Character, ( Control Character &#124; "n" &#124; "r" &#124; "t" &#124; Hex Sequence &#124; Unicode Sequence ) ; |
| Hex Sequence     | = | "x", Hex Character, Hex Character ;                                                                                    |
| Unicode Sequence | = | "u", Open Token, Hex Character, 5 * [ Hex Character ], Close Token ;                                                   |

##### Whitespace Token

Whitespace Tokens are composed of a series of one or more Whitespace Characters.

|                  |   |                                                  |
|------------------|---|--------------------------------------------------|
| Whitespace Token | = | Whitespace Character, { Whitespace Character } ; |

##### Text Token

Text Tokens are composed of a series of 1 or more Text Characters or Whitespace Characters, with at least one Text Character.

|            |   |                                                                                            |
|------------|---|--------------------------------------------------------------------------------------------|
| Text Token | = | { Whitespace Character }, Text Character, { Whitespace Character &#124; Text Character } ; |

##### Special Token

Special Tokens are composed of an Escape Character followed by a Special Character.

|               |   |                                       |
|---------------|---|---------------------------------------|
| Special Token | = | Escape Character, Special Character ; |

#### Document Structure

##### Whitespace

Optional Whitespace is composed of zero or more alternating Whitespace Tokens and runs of Comment Tokens. The value of Optional Whitespace is equivalent to the concatenation of the values of all the Whitespace Tokens that comprise it.

|                     |   |                                                                 |
|---------------------|---|-----------------------------------------------------------------|
| Optional Whitespace | = | [ Whitespace Token ], { Comment Token, [ Whitespace Token ] } ; |

##### Text

Basic Text is composed of a sequence of any number of Whitespace, Text Characters, and Escape Sequences, with at least one non-Whitespace element. The value of Basic Text is equivalent to the concatenation of the values of all the Whitespace, Text Characters, and Escape Sequences that comprise it.

|                        |   |                                                                                                                                  |
|------------------------|---|----------------------------------------------------------------------------------------------------------------------------------|
| Basic Text             | = | { [ Whitespace Token ], Comment Token }, ( Text Sequence &#124; Escape Sequence ) ;                                              |
| Comment Sequence       | = | Comment Token, [ Comment Sequence &#124; Escape Sequence &#124; Whitespace Sequence &#124; Text Sequence ] ;                     |
| Escape Sequence        | = | Escape Token, [ Comment Sequence &#124; Escape Sequence &#124; Whitespace Sequence &#124; Text Sequence ] ;                      |
| Whitespace Sequence    | = | Whitespace Token, [ Comment Sequence &#124; Escape Sequence ] ;                                                                  |
| Text Sequence          | = | Text Token, [ Comment Sequence &#124; Escape Sequence ] ;                                                                        |

Enclosed Text Sequences are a series of one or more of Enclosed Texts optionally separated by Whitespace. The value of an Enclosed Text Sequence is equivalent to the concatenation of the values of all the Enclosed Texts that comprise it.

Enclosed Text is composed of an Open Token, followed by a Whitespace Token or Text, followed by a Close Token. The value of Enclosed Text is equivalent to the Whitespace or Text component.

|                        |   |                                                                                                                                  |
|------------------------|---|----------------------------------------------------------------------------------------------------------------------------------|
| Enclosed Text Sequence | = | Enclosed Text, { Optional Whitespace, Enclosed Text } ;                                                                          |
| Enclosed Text          | = | Open Token, ( Whitespace Token &#124; Text ), Close Token ;                                                                      |

Text is composed of a series of one or more alternating Enclosed Text Sequences and Basic Text. Text may start or end with either of these elements. A series that begins with an Enclosed Text Sequence may optionally be preceded by Whitespace. Similarly, a series that ends with an Enclosed Text Sequence may optionally be followed by Whitespace. The value of Text is equivalent to the concatenation of the values of all the Enclosed Text Sequence and Basic Text that comprise it.

|                        |   |                                                                                                                                  |
|------------------------|---|----------------------------------------------------------------------------------------------------------------------------------|
| Text                   | = | Leading Basic Text &#124; Leading Enclosed Text ;                                                                                |
| Leading Basic Text     | = | Basic Text, { Enclosed Text Sequence, Basic Text }, [ Enclosed Text Sequence, Optional Whitespace ] ;                            |
| Leading Enclosed Text  | = | Optional Whitespace, Enclosed Text Sequence, { Basic Text, Enclosed Text Sequence }, ( Optional Whitespace &#124; Basic Text ) ; |

##### Tuple

A Tuple falls into one of two distinct types: Monad, and List Tuple.

|                    |   |                           |
|--------------------|---|---------------------------|
| Tuple              | = | Monad &#124; List Tuple ; |

Monads can either be Text Monads, Whitespace Monads, or Special Monads. Text Monads have a value equivalent to the Text that comprise it. Whitespace Monads have a value equivalent to the Optional Whitespace that comprise it. Special Monads have a value based on the significance of the Special Token.

|                    |   |                                                           |
|--------------------|---|-----------------------------------------------------------|
| Monad              | = | Text Monad &#124; Whitespace Monad &#124; Special Monad ; |
| Text Monad         | = | Text ;                                                    |
| Whitespace Monad   | = | Optional Whitespace ;                                     |
| Special Monad      | = | Optional Whitespace, Special Token, Optional Whitespace ; |

List Tuples are composed of Optional Whitespace, followed by an Open Token, followed by either List Elements, a List Tuple, a Special Monad, or nothing, followed by a Close Token, followed by Optional Whitespace.

List Elements are a series of one or more Tuples separated by Divider Tokens, with a trailing Divider Token and Optional Whitespace being optional. Note that Whitespace Monads must be followed by a Divider Token.

|                    |   |                                                                                                                                                     |
|--------------------|---|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| List Tuple         | = | Optional Whitespace, Open Token, [ List Elements | List Tuple | Special Monad ], Close Token, Optional Whitespace ;                                 |
| List Elements      | = | Tuple, Divider Token, ( Optional Whitespace &#124; List Element &#124; Whitespace Element ) ;                                                       |
| List Element       | = | ( Text Monad &#124; Special Monad &#124; List Tuple ), [ Divider Token, ( Optional Whitespace &#124; List Element &#124; Whitespace Element ) ] ) ; |
| Whitespace Element | = | Whitespace Monad, Divider Token, [ Optional Whitespace &#124; List Element &#124; Whitespace Element ] ;                                            |

##### Document

A document is simply comprised of a single Tuple.

|          |   |         |
|----------|---|---------|
| Document | = | Tuple ; |
