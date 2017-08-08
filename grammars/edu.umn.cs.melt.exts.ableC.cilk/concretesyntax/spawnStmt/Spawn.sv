grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:spawnStmt;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;


marking terminal CilkSpawn_t 'spawn' lexer classes {Ckeyword};

concrete production cilk_spawn_c
top::Stmt_c ::= 'spawn' sb::SpawnBody
{
  top.ast = sb.ast;
}

nonterminal SpawnBody with location, ast<abs:Stmt> ;
concrete productions sb::SpawnBody
| l::UnaryExpr_c  op::AssignOp_c  f::PostfixExpr_c '(' args::ArgumentExprList_c ')' ';'
  {
    sb.ast =
      cilkSpawnStmt(
        l.ast,
        op.ast,
        f.ast,
        abs:foldExpr(args.ast)
      );
  }

| l::UnaryExpr_c op::AssignOp_c f::PostfixExpr_c '(' ')' ';'
  {
    sb.ast =
      cilkSpawnStmt(
        l.ast,
        op.ast,
        f.ast,
        abs:nilExpr()
      );
  }

| f::PostfixExpr_c '(' args::ArgumentExprList_c ')' ';'
  {
    sb.ast =
      cilkSpawnStmtNoEqOp(
        f.ast,
        abs:foldExpr(args.ast)
      );
  }

| f::PostfixExpr_c '(' ')' ';'
  {
    sb.ast =
      cilkSpawnStmtNoEqOp(
        f.ast,
        abs:nilExpr()
      );
  }
