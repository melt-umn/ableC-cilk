grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;

-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

concrete production cilk_return_c
top::Stmt_c ::= 'cilk' rb::ReturnBody
{
  top.ast = rb.ast;
}

nonterminal ReturnBody with location, ast<abs:Stmt> ;
concrete productions rb::ReturnBody
| 'return' ';'
    { rb.ast = cilk_returnStmt(abs:nothingExpr()); }
| 'return' rv::Expr_c ';'
    { rb.ast = cilk_returnStmt(abs:justExpr(rv.ast)); }

