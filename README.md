An interpreter for a stack-based, concatenative language inspired by [Factor](http://factorcode.org).

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
