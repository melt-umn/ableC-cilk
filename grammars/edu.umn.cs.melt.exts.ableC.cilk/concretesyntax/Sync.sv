grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

marking terminal CilkSync_t 'sync' lexer classes {Ckeyword};

concrete production cilk_sync_c
top::Stmt_c ::= 'sync' ';'
{
  top.ast = cilk_syncStmt(top.location);
}

