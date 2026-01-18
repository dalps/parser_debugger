%[@break "foo"]

%token <int> INT [@break 123]
%token PLUS [@break] MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main

%%

main:
| e = expr EOF
    { e }

expr [@break rule]:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr [@break producer] TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 } [@break production]
| MINUS e = expr %prec UMINUS
    { - e }

