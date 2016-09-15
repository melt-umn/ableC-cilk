grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_exitStmt
e::Stmt ::= me::MaybeExpr
{
  -- really turn this into the funny do-while(0) found in cilk-lib.cilkh

  forwards to 
    exprStmt (
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

