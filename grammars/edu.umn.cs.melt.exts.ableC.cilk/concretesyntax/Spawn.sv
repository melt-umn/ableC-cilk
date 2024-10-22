grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;


marking terminal CilkSpawn_t 'spawn' lexer classes {Keyword, Global};

concrete production cilk_spawn_c
top::Stmt_c ::= 'spawn' sb::SpawnBody
{
  top.ast = sb.ast;
}

nonterminal SpawnBody with ast<abs:Stmt> ;
concrete productions sb::SpawnBody
| l::UnaryExpr_c  op::AssignOp_c  f::PostfixExpr_c '(' args::ArgumentExprList_c ')' ';'
  {
    sb.ast = cilkSpawnStmt(l.ast, f.ast, abs:foldExpr(args.ast));
  }

| l::UnaryExpr_c op::AssignOp_c f::PostfixExpr_c '(' ')' ';'
  {
    sb.ast = cilkSpawnStmt(l.ast, f.ast, abs:nilExpr());
  }

| f::PostfixExpr_c '(' args::ArgumentExprList_c ')' ';'
  {
    sb.ast = cilkSpawnStmtNoEqOp(f.ast, abs:foldExpr(args.ast));
  }

| f::PostfixExpr_c '(' ')' ';'
  {
    sb.ast = cilkSpawnStmtNoEqOp(f.ast, abs:nilExpr());
  }

