grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:returnStmt;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;

-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

exports edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:cilkKeyword;

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

