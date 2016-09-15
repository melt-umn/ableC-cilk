grammar edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:exitStmt ;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;


marking terminal CilkExit_t 'cilk_exit' lexer classes {Ckeyword};

{- Cilk_exit in the MIT Cilk implementation is a macro generating
   a do-while construct.  Thus it is a statement, specifically an
   InterationStmt_c.
-}
concrete production cilk_exit_c
top::IterationStmt_c ::= 'cilk_exit' rb::ExitBody
{
  top.ast = rb.ast;
}

nonterminal ExitBody with location, ast<abs:Stmt> ;
concrete productions rb::ExitBody
| ';'
    { rb.ast = cilk_exitStmt(abs:nothingExpr()); }
| rv::Expr_c ';'
    { rb.ast = cilk_exitStmt(abs:justExpr(rv.ast)); }

