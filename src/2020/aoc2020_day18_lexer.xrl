Definitions.

Rules.

[\000-\s]+ :
  skip_token.

\* :
  {token, {mult_op, TokenLine}}.

\+ :
  {token, {plus_op, TokenLine}}.

\( :
  {token, {'(', TokenLine}}.

\) :
  {token, {')', TokenLine}}.

[0-9]+ :
  {token, {'int', list_to_integer(TokenChars)}}.

Erlang code.

%% @hidden
