grammar edu:umn:cs:melt:exts:ableC:cilk;

import edu:umn:cs:melt:exts:ableC:parallel;
import edu:umn:cs:melt:ableC:abstractsyntax:construction;
import edu:umn:cs:melt:ableC:abstractsyntax:env;
import edu:umn:cs:melt:ableC:abstractsyntax:host;

aspect production parallelism
top::ParallelConstruct ::= env::Decorated Env nproc::Expr
{
  local cilkLoc :: Location = builtinLoc("cilk");

  top.parallelExtensions <-
    [parallelExtension(MODULE_NAME, nothing(), nothing(),
     \e::Expr num::Integer -> 
      exprStmt(directCallExpr(name("init_cilk_ableC", location=cilkLoc),
        consExpr(e, consExpr(mkIntConst(num, cilkLoc), nilExpr())), location=cilkLoc)))];
}
