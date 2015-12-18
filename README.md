# sopt
```
                 _   
 ___  ___  _ __ | |_ 
/ __|/ _ \| '_ \| __|
\__ \ (_) | |_) | |_ 
|___/\___/| .__/ \__|
          |_|
```

A Scheme Source-Code Optimizer implemented by Gauche.

## Algorithm
* Online Partial Evaluation
* Positive Supercompilation

## Syntax
```
def     ::= (define (name args ...) terms ...)

var     ::= symbol

literal ::= symbol | ...            // Gauche-objects

term    ::= var                     [var]
          | literal
          | (quote literal)         [literal]
          | (if test-term
                then-term
                else-term)          [if]
          | (let
              ((var1 term1)
               ...
               (varN-1 termN-1))
              terms ...)            [let]
          | (apply term lst)        [apply]
          | (lambda args terms ...) [lambda]
          | (call/cc term)          [call/cc]
          | (set! var term)         [set!]
          | (term1 ...)             [call]
```

## Example
```scm
% gosh main.scm -h

                 _   
 ___  ___  _ __ | |_ 
/ __|/ _ \| '_ \| __|
\__ \ (_) | |_) | |_ 
|___/\___/| .__/ \__|
          |_|

Usage: sopt [--in=file] [--out=file] [-e|--ext] [--fun=name] [--args=args]

% echo '(define (f x y) (+ x y))' | gosh main.scm --fun=f --args='(a b)'
(define (f a b) (+ a b))
% echo '(define (f x y) (+ x y))' | gosh main.scm --fun=f --args='(1 2)' -e
(define (f--sopt--0) '3)
```

## References
* M.H. Sørensen, R. Gluck, and N.D. Jones. 1993. A Positive Supercompiler.
* N.D. Jones, C.K. Gomard, and P. Sestoft. 1993. Partial Evaluation and Automatic Program Generation. Prentice-Hall.
* Richard Kelsey, Will Clinger, Jonathan Rees (editors). 1998. Revised5 Report on the Algorithmic Language Scheme. Higher-Order and Symbolic Computation.
