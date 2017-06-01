grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:syncStmt;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

marking terminal CilkSync_t 'sync' lexer classes {Ckeyword};

concrete production cilk_sync_c
top::Stmt_c ::= 'sync' ';'
{
  top.ast = cilk_syncStmt(top.location);
}

