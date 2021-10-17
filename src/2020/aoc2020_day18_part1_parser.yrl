%% Header "%% @hidden".

Nonterminals
   expr.

Terminals
   '(' ')' int plus_op mult_op.

Rootsymbol expr.

Nonassoc 100 '('.
Nonassoc 100 ')'.
Left      40 plus_op.
Left      40 mult_op.

expr -> int : {expr, '$1'}.
expr -> '(' expr ')' : {expr, '$2'}.
expr -> expr plus_op expr : {plus, '$1', '$3'}.
expr -> expr mult_op expr : {mult, '$1', '$3'}.
