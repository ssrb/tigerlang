%token <string> ID
%token <string> STRING
%token <int> INT
%token EOF
%token COMMA 
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK 
%token LBRACE
%token RBRACE
%token DOT 
%token OP_PLUS
%token OP_MINUS
%token OP_MUL
%token OP_DIV
%token OP_EQ
%token OP_NEQ
%token OP_LT
%token OP_LE
%token OP_GT
%token OP_GE
%token OP_AND
%token OP_OR
%token ASSIGN
%token ARRAY
%token IF
%token THEN
%token ELSE
%token WHILE
%token FOR
%token TO
%token DO
%token LET
%token IN
%token END
%token OF 
%token BREAK
%token NIL
%token FUNCTION
%token VAR
%token TYPE 
%start <unit> dec
%%

decs: dec* { () };

dec: tydec /*| vardec | fundec*/ { () };

tydec: TYPE; typeid; OP_EQ; ty { () };

typeid: ID { () };

ty: typeid | LBRACE; typefields; RBRACE | ARRAY; OF; typeid { () };
 
typefield: ID; COLON; typeid { () };

typefields: separated_list(COMMA, typefield) { () };