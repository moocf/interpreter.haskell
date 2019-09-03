try to implement 3 operators only, and directly use racket lists (s-exp)
and not string, to avoid getting into scanning. the first step involves
defining the structure (datatype) of your AST. then think of how you can
recursively evaluate an AST to get the answer (number?). once you are able
to evaluate ASTs, you would want to actually be able to parse s-exp to
generate the AST. this is pretty easy since s-exps are already quite close
to what an AST looks like.

for now assume only binary operators, we will get to unary operators in a
later exercise.

```
e ::= (+ e e) | (- e e) | (* e e) | n

e: expression
n: number
```
