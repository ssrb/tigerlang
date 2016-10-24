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
%token PLUS
%token MINUS
%token MUL
%token DIV
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
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

%right THEN ELSE DO OF ASSIGN
%left AND OR
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MUL DIV
%start <unit> prog
%%

prog: exp; EOF; { () };

decs: dec* { () };

dec: tydec | vardec | fundec { () };

%inline typeid: ID { () };

tydec: TYPE; typeid; EQ; ty { () };

ty: typeid | LBRACE; typefields; RBRACE | ARRAY; OF; typeid { () };
 
typefield: ID; COLON; typeid { () };

typefields: separated_list(COMMA, typefield) { () };

vardec: VAR; ID; ASSIGN; exp | VAR; ID; COLON; typeid; ASSIGN; exp { () };

fundec: | FUNCTION; ID; LPAREN; typefields; RPAREN; EQ; exp
        | FUNCTION; ID; LPAREN; typefields; RPAREN; COLON; typeid; EQ; exp { () };

bracketed: ID; LBRACK; exp; RBRACK; { () };

lvalue: | ID
        | lvalue; DOT; ID
        | bracketed { () };

exp:  | lvalue
      | NIL
      | LPAREN; separated_list(SEMICOLON, exp); RPAREN;
      | INT
      | STRING
      | MINUS; exp 
      | ID; LPAREN; separated_list(COMMA, exp); RPAREN 
      | exp; PLUS; exp
      | exp; MINUS; exp
      | exp; MUL; exp
      | exp; DIV; exp
      | exp; EQ; exp
      | exp; NEQ; exp
      | exp; LT; exp
      | exp; LE; exp
      | exp; GT; exp
      | exp; GE; exp
      | exp; AND; exp
      | exp; OR; exp
      | typeid; LBRACE; separated_list(COMMA, ID; EQ; exp { () }); RBRACE
      | bracketed; OF; exp
      | lvalue; ASSIGN; exp
      | IF; exp; THEN; exp
      | IF; exp; THEN; exp; ELSE; exp
      | WHILE; exp; DO; exp
      | FOR; ID; ASSIGN; exp; TO; exp; DO; exp
      | LET; decs; IN; separated_list(SEMICOLON, exp); END
      | BREAK
       { () };
