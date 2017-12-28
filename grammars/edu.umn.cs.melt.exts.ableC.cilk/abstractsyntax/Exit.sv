grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

abstract production cilk_exitStmt
s::Stmt ::= me::MaybeExpr
{
  s.functionDefs := [];

  local retval :: Exprs =
    case me.justTheExpr of
    | just(e)   -> consExpr(e, nilExpr())
    -- TODO: do we really want to default to exit 0?
    | nothing() -> consExpr(mkIntConst(0, builtinLoc(MODULE_NAME)), nilExpr())
    end;
  retval.env = s.env;

  -- do { spawn Cilk_really_exit(ret); } while (0)
  forwards to
    doStmt(
      cilkSpawnStmtNoEqOp(
        declRefExpr(name("Cilk_really_exit", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
        retval
      ),
      mkIntConst(0, builtinLoc(MODULE_NAME))
    );
}

