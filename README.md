A stack-based, concatenative language inspired by Factor.

```
> cat ./samples/fibonacci.stack

: fib
    dup 2 < [
        [ 1 - fib ] [ 2 - fib ] bi +
    ] unless ;

15 fib .

> ./StackLang ./samples/fibonacci.stack

610
```
