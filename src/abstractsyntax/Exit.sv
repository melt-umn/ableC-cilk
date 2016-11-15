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

  -- TODO: allow spawn w/o assignOp, allow cilk function prototypes
--  local retval :: Exprs =
--    case me.justTheExpr of
--    | just(e)   -> consExpr(e, nilExpr())
--    | nothing() -> nilExpr()
--    end;
--
--  -- do { spawn Cilk_really_exit(ret); } while (0)
--  forwards to
--    doStmt(
--      foldStmt([
--        mkIntDecl("_unused", builtIn()),
--        cilkSpawnStmt(
--          declRefExpr(name("_unused", location=builtIn()), location=builtIn()),
--          eqOp(location=builtIn()),
--          declRefExpr(name("Cilk_really_exit", location=builtIn()), location=builtIn()),
--          retval
--        )
--      ]),
--      mkIntConst(0, builtIn())
--    );

  forwards to
    exprStmt(
      directCallExpr(
        name("exit", location=loc("on cilk exit", -1, -1, -1, -1, -1, -1)),
                     --ToDo: when Stmt gets location, change this to e.location
        case me.justTheExpr of
        | just(e) -> consExpr(e, nilExpr())
        | nothing() -> nilExpr()
        end,
        location=loc("on cilk exit", -1, -1, -1, -1, -1, -1)
        --ToDo: when Stmt gets location, change this to e.location
      )
    );
}

