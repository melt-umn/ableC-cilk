grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;

-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

concrete productions top::IterationStmt_c
| 'cilk' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' body::CilkForBody_c
  {
    top.ast = cilkForStmt(init.asMaybeExpr, cond.asMaybeExpr, abs:justExpr(iter.ast), body.ast);
  }
| 'cilk' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' body::CilkForBody_c
  {
    top.ast = cilkForStmt(init.asMaybeExpr, cond.asMaybeExpr, abs:nothingExpr(), body.ast);
  }
| 'cilk' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' body::CilkForBody_c
  {
    top.ast = cilkForDeclStmt(init.ast, cond.asMaybeExpr, abs:justExpr(iter.ast), body.ast);
  }
| 'cilk' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' body::CilkForBody_c
  {
    top.ast = cilkForDeclStmt(init.ast, cond.asMaybeExpr, abs:nothingExpr(), body.ast);
  }


closed nonterminal CilkForBody_c with ast<abs:Stmt>;

concrete productions top::CilkForBody_c
| '{' f::PostfixExpr_c '(' args::ArgumentExprList_c ')' ';' '}'
  {
    top.ast = cilkForBody(f.ast, abs:foldExpr(args.ast));
  }

