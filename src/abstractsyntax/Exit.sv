grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_exitStmt
e::Stmt ::= me::MaybeExpr
{
  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  e.globalDecls := [];
  e.defs = [];
  e.freeVariables = [];
  e.functiondefs = [];

  local retval :: Exprs =
    case me.justTheExpr of
    | just(e)   -> consExpr(e, nilExpr())
    | nothing() -> nilExpr()
    end;
  retval.env = e.env;

  -- do { spawn Cilk_really_exit(ret); } while (0)
  forwards to
    doStmt(
      cilkSpawnStmtNoEqOp(
        declRefExpr(name("Cilk_really_exit", location=builtIn()), location=builtIn()),
        retval
      ),
      mkIntConst(0, builtIn())
    );
}

