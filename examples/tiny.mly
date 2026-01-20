%token Ta "a" Tb "b" Tc "c" EOF

%start <unit> main
%start <unit> rule_S

%%

main: rule_S EOF {}

rule_S:
  "a" rule_A "c" {}

rule_A:
  | rule_A "b" "b" {}
  | "b" {}