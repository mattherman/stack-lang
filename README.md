An interpreter for a stack-based, concatenative language inspired by [Factor](http://factorcode.org). Factor is a Forth-like language.

## Introduction

Stack-based languages operate on a stack of values. Executing a normal value will push it onto the top of the stack. The language also provides various operations for manipulating the values on the stack.

Operations pop their parameters from the stack and push their return values onto the stack. For example, the `+` operation will pop the top two values from the stack, add them, and then push the result:

```
> 2 6 +

--- Data stack:
8
```

This is referred to as postfix or Reverse Polish notation.

The concatenative nature of the language means that concatenating two programs results in the composition of those two programs.

For example, if you had two operations, one that multiplied a value by 2 (`2 *`) and one that added 3 (`3 +`), you could compose those two operations by simply concatenating them together (`2 * 3 +`).

Operations or functions in Forth-like languages are often called "words" and the list of defined words is the "vocabulary". You add a word to the vocabulary with the following syntax:

```
: <name> <..body..> ;
```

For example, the following defines a word named `square` which duplicates the value on the top of the stack and then multiplies them together:

```
: square dup * ;
```

It can then be executed:

```
> 5 square .
25
```

(Note: the `.` operation simply pops the top value from the stack and displays it)

## Examples

```
: square dup * ;
: even? % 2 = 0 ;

{ 1 2 3 4 5 } [ even? ] filter [ square ] map .

=> { 4 16 }
```

```
: fib
    dup 2 < [
        [ 1 - fib ] [ 2 - fib ] bi +
    ] unless ;

15 fib .

=> 610
```

See [StackLang.Interpreter.Tests/Tests.fs](https://github.com/mattherman/stack-lang/blob/main/StackLang.Interpreter.Tests/Tests.fs) for more examples.
