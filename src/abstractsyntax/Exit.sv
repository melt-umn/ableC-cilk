grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_exitStmt
s::Stmt ::= me::MaybeExpr
{
  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs := [];
  s.freeVariables = [];
  s.functiondefs := [];

  local retval :: Exprs =
    case me.justTheExpr of
    | just(e)   -> consExpr(e, nilExpr())
    -- TODO: do we really want to default to exit 0?
    | nothing() -> consExpr(mkIntConst(0, bogusLoc()), nilExpr())
    end;
  retval.env = s.env;

  -- do { spawn Cilk_really_exit(ret); } while (0)
  forwards to
    doStmt(
      cilkSpawnStmtNoEqOp(
        declRefExpr(name("Cilk_really_exit", location=bogusLoc()), location=bogusLoc()),
        retval
      ),
      mkIntConst(0, bogusLoc())
    );
}

