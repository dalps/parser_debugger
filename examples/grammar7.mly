%token Ta "a" Tb "b" Tc "c" EOF

%start <unit> main
%start <unit> rule_S

%nonassoc prec_B
%nonassoc Tb

%%

main: rule_S EOF {}

rule_S:
  "a" rule_A "c" {}

rule_A:
  | "b" rule_A "b" {}
  | "b" {} %prec prec_B