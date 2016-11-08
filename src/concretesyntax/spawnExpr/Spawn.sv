grammar edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:spawnExpr ;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;


marking terminal CilkSpawn_t 'spawn' lexer classes {Ckeyword};


concrete production cilk_spawn_c
top::AssignExpr_c ::= 'spawn' sb::SpawnBody
{
  top.ast = sb.ast;
}

nonterminal SpawnBody with location, ast<abs:Expr> ;
concrete productions sb::SpawnBody
| l::UnaryExpr_c  eq::'='  f::PostfixExpr_c '(' args::ArgumentExprList_c ')'
  {
    sb.ast =
      abs:stmtExpr(
        cilkSpawnStmt(
          l.ast,
          abs:eqOp(location=eq.location),
          f.ast,
          abs:foldExpr(args.ast)
        ),
        l.ast,
        location=sb.location
      );
  }

| l::UnaryExpr_c eq::'=' f::PostfixExpr_c '(' ')'
  {
    sb.ast =
      abs:stmtExpr(
        cilkSpawnStmt(
          l.ast,
          abs:eqOp(location=eq.location),
          f.ast,
          abs:nilExpr()
        ),
        l.ast,
        location=sb.location
      );
  }

