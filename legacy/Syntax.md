### Basic introduction

All values in the shell are roughly based on JSON
A value is:
```
| Nil
| True
| False
| String
| Number
| Array of other values
| Object of string keys and other values
| Function
```

Binding variables + examples of each type of value:
```
let $nil = Nil

let $true = True

let $false = False

let $string = "Hello world!"

let $number = 3.1415926

let $array = [ "You can mix types", 5.2, [1,2,3], Nil ]

let $object = { "quoted key": 5, unquoted_key: "hello world" }

let $function = |arg1, arg2| -> $arg1 + $arg2
```

#### Objects

It is recommended to use keys without spaces in, as then you can do `$object.unquoted_key` to access the unquoted key value

If you need spaces for whatever reason the syntax `$object["quoted key"]` works

Objects are immutable, you cannot assign a new key/value to an object after it is created

#### Arrays

You can use typical indexer syntax `$array[1]`

Array indexes start at 0

If you pass a non-integer as an array index, it will be rounded (nearest number, nearest even number for midpoints)

Arrays are immutable, you cannot assign a new value to an element of an array

#### Functions

They are the core point of the shell. Any functions declared in the current scope can be executed by typing their name without `$`
```
let $myfunc = |x, y| -> $x + $y

myfunc 5 (4 + 10)
```
Arguments are space separated, use brackets to provide an expression as an argument

This would output 19 in the repl

You can invoke any expression as a function using brackets syntax:
`$myfunc(5, 10)`
This applies to any expression so for example
```
let $k = |x| -> || -> $x

let $arr = [k 1, k 2, k 3]

$arr[1]()
```
This would output 2

Functions can return functions and take them as arguments, since they are values

Remember: it is defining a variable name with the function value that lets you call it just by the name

### Expressions

You've already seen object and array indexing above, and the + operator

Here's a full list of unary operators:
- `-` negates a number if placed before it
- `~` is the rounding operator, and rounds a number to the nearest integer (nearest even integer for midpoints)

  Used implicitly for array indices.

- `!` is the boolean NOT operator
- `?` is the truth operator - It converts values of other types to True if they are truthy and False if they are falsy

  Used implicitly for `if` statement conditions.
  
  Truthy values: Nonempty strings, Nonzero numbers, Nonempty arrays, True
  
  Falsy values: Empty strings, 0, [], Nil, False
  
  Objects and functions are not truthy or falsy
- `@` is the string operator. Converts any expression into a string equivalent
  Used implicitly by the `@:` operator.
- `@:` is the print operator. Prints the value of the expression to the console, and then returns it.

Here's a full list of binary operators:

`+`, `-`, `*`, `/` do what you expect, accepting numbers only

`||`, `&&` are binary OR and AND, accepting boolean values only

`|` is the pipeline operator

#### The pipeline operator

My favourite feature, the pipeline operator lets you chain actions together

Best shown by example
`5 | $ * 2 | [$, $+1, $+2] | $[2] + 100`
Would output 112

The pipeline is evaluated from left to right, starting with the original expression and then substituting the result as `$` into the next

The new result is substituted as `$` next and so on

Very useful for batch processing arrays/general scripty things

### Other constructs

If expressions
```
let $obj = { x: True, y: False }
if $obj.x then 6 elif $obj.y then 7 else 8
```
If-expressions are exactly that: expressions. Each "arm" of the if statement is an expression that is returned overall.

They always need an else case (in the future it will probably just treat missing one as `else Nil`)

Block expressions
```
{
    let $x = 100;
    $x * $x
}
```
Blocks consist of multiple statements like variable declarations, commands, etc, and then finish with an expression which is returned.

All variables bound inside a block statement are local to that block. After the block completes they are forgotten.

Blocks with if expressions:

```
if ("Strings are truthy") then
{
    let $x = 100;
    let $y = 200;
    $x | $ + $y | $ + 1
}
else { let $s = "Hello!"; $s }
```

### Side effects

You may have noticed that the shell has no side effects on its own

Indeed, the outside environment (F#) must provide it with some commands that do have side effects

I guess I will write that up in another few months
