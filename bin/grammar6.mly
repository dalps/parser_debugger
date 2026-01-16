%token Ta "a" Tb "b" Tc "c" EOL FIN

%start <unit> main
%start <unit> rule_S

// Swap these two to match the graph of grammar7.mly
%nonassoc Rule_b
%nonassoc Tb

%%

main: rule_S EOL {}

rule_S:
  "a" rule_A "c" {}

rule_A:
  | "b" rule_A "b" {}
  | "b" {} %prec Rule_b