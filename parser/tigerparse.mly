%{
module A = Absyn
module S = Symbol
%}

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
%left negation
%start <Absyn.exp> prog
%%

prog: e = exp EOF { e }

decs: d = list(dec) { d }

dec: tydec | vardec | fundec { A.NilExp }

%inline typeid: s = ID { s }

tydec: TYPE typeid EQ ty { A.NilExp }

ty: typeid | LBRACE typefields RBRACE | ARRAY OF typeid { A.NilExp }
 
typefield: fname = ID c = COLON tname = typeid { ({ name = S.symbol fname; escape = ref false; typ = S.symbol tname; pos = $startpos(c) } : A.field) }

typefields: fs = separated_list(COMMA, typefield) { fs }

vardec: VAR ID ASSIGN exp | VAR ID COLON typeid ASSIGN exp { A.NilExp }

fundec: FUNCTION fname = ID LPAREN p = typefields RPAREN r = option(COLON t = typeid{ (S.symbol t, $startpos(t)) }) EQ b = exp 
{ A.FunctionDec([{ name = S.symbol fname; params = p; result = r; body = b; pos = $startpos }]) }

lvalue: | vname = ID { A.SimpleVar(S.symbol vname, $startpos) }
        | v = lvalue d = DOT fname = ID { A.FieldVar(v, S.symbol fname, $startpos(d)) }
        | v = lvalue LBRACK e = exp RBRACK { A.SubscriptVar(v, e, $startpos) }

%inline op: | PLUS { A.PlusOp }
            | MINUS { A.MinusOp }
            | MUL { A.MulOp }
            | DIV { A.DivOp }
            | EQ { A.EqOp }
            | NEQ { A.NeqOp }
            | LT { A.LtOp }
            | LE { A.LeOp } 
            | GT { A.GtOp }
            | GE { A.GeOp }

exp:  | lval = lvalue { A.VarExp(lval) }
      | NIL  { A.NilExp }
      | LPAREN seq = separated_list(SEMICOLON, e = exp { (e, $startpos) }) RPAREN { A.SeqExp(seq) }
      | i = INT { A.IntExp(i) }
      | s = STRING { A.StringExp(s, $startpos) }
      | MINUS e = exp %prec negation { OpExp({ left = A.IntExp(0); oper = A.MinusOp; right = e; pos = $startpos }) }
      | fname = ID LPAREN args = separated_list(COMMA, exp) RPAREN  { A.CallExp({ func = S.symbol fname; args = args; pos = $startpos}) }
      | l = exp op = op r = exp { A.OpExp({ left = l; oper = op; right = r; pos = $startpos(op) }) }
      | l = exp op = AND r = exp { A.IfExp({ test = l; then' = r; else' = Some(A.IntExp(0)); pos = $startpos(op) }) }
      | l = exp op = OR r = exp { A.IfExp({ test = l; then' = A.IntExp(1); else' = Some(r); pos = $startpos }) }
      | typeid LBRACE separated_list(COMMA, ID EQ exp { () }) RBRACE { A.NilExp }
      | ID LBRACK exp RBRACK OF exp { A.NilExp }
      | v = lvalue a = ASSIGN e = exp { A.AssignExp({ var = v; exp = e; pos = $startpos(a) }) }
      | IF test = exp THEN e = exp { A.IfExp({ test = test; then' = e; else' = None; pos = $startpos }) }
      | IF test = exp THEN e1 = exp ELSE e2 = exp { A.IfExp({ test = test; then' = e1; else' = Some(e2); pos = $startpos }) }
      | WHILE test = exp DO e = exp { A.WhileExp({ test = test; body = e; pos = $startpos }) }
      | FOR vname = ID ASSIGN lo = exp TO hi = exp DO b = exp { A.ForExp({ var = S.symbol(vname); escape = ref false; lo = lo; hi = hi; body = b; pos = $startpos }) }
      | LET d = decs IN b = separated_list(SEMICOLON, exp) END { A.NilExp }
      | BREAK { A.BreakExp($startpos) }
