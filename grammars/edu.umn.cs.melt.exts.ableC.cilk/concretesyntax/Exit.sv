grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

marking terminal CilkExit_t 'cilk_exit' lexer classes {Ckeyword};

{- Cilk_exit in the MIT Cilk implementation is a macro generating
   a do-while construct.  Thus it is a statement, specifically an
   IterationStmt_c.
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

