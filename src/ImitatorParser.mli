type token =
  | INT of (NumConst.t)
  | FLOAT of (string)
  | NAME of (string)
  | STRING of (string)
  | OP_PLUS
  | OP_MINUS
  | OP_MUL
  | OP_DIV
  | OP_L
  | OP_LEQ
  | OP_EQ
  | OP_GEQ
  | OP_G
  | OP_ASSIGN
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQBRA
  | RSQBRA
  | AMPERSAND
  | APOSTROPHE
  | COLON
  | COMMA
  | PIPE
  | SEMICOLON
  | CT_AND
  | CT_AUTOMATON
  | CT_ANALOG
  | CT_BAD
  | CT_CLOCK
  | CT_DISCRETE
  | CT_DO
  | CT_END
  | CT_ENDREACH
  | CT_FALSE
  | CT_FORWARD
  | CT_FROM
  | CT_GOTO
  | CT_IF
  | CT_INIT
  | CT_INITIALLY
  | CT_IN
  | CT_LOC
  | CT_LOCATIONS
  | CT_NOT
  | CT_OR
  | CT_PARAMETER
  | CT_PRINT
  | CT_REACH
  | CT_REGION
  | CT_SYNC
  | CT_SYNCLABS
  | CT_TRUE
  | CT_VAR
  | CT_WAIT
  | CT_WHEN
  | CT_WHILE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParsingStructure.parsing_structure
