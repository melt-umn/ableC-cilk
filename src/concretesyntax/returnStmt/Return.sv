grammar edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:returnStmt ;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;


marking terminal CilkReturn_t 'cilk_return' lexer classes {Ckeyword};


concrete production cilk_return_c
top::Stmt_c ::= 'cilk_return' rb::ReturnBody
{
  top.ast = rb.ast;
}

nonterminal ReturnBody with location, ast<abs:Stmt> ;
concrete productions rb::ReturnBody
| ';'
    { rb.ast = cilk_returnStmt(abs:nothingExpr()); }
| rv::Expr_c ';'
    { rb.ast = cilk_returnStmt(abs:justExpr(rv.ast)); }

