grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilkSpawnStmt
s::Stmt ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), l.pp, space(), op.pp, space(),
                  f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
--  s.globalDecls := [];
--  s.defs = [];
--  s.freeVariables = [];
--  s.functiondefs = [];

  -- reserve a sync number
  s.syncCount = s.syncCountInh + 1;

  -- TODO: transform args

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      args
    );

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(s.syncCount);

  -- TODO: save dirty variables

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtIn())
    | _               -> callExpr(f, newArgs, location=builtIn())
    end;

  forwards to
    foldStmt([
      setHeaderEntry,
      case fast,slow of
      | true,false  -> cilk_fastCloneSpawnWithEqOp(l, op, callF)
      | false,true  -> cilk_slowCloneSpawnWithEqOp(l, op, callF, s.syncCount)
      | true,true   -> error ("We think we're in both a fast and a slow clone!")
      | false,false -> error ("We don't think we're in a fast or slow clone!")
      end
    ]);
}

abstract production cilk_fastCloneSpawnWithEqOp
s::Stmt ::= l::Expr op::AssignOp callF::Expr
{
  --s.errors := [] ; -- TODO .... l.type   ++ f.erros ++ args.errors ;

  -- l = callF();
  local assignExpr :: Expr =
    binaryOpExpr(
      l,
      assignOp(op, location=builtIn()),
      callF,
      location=builtIn()
    );

  forwards to cilk_fastCloneSpawn(assignExpr, justExpr(l));
}

abstract production cilkSpawnStmtNoEqOp
s::Stmt ::= f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  -- TODO: refactor this to reuse cilkSpawnStmt code

  -- reserve a sync number
  s.syncCount = s.syncCountInh + 1;

  -- TODO: transform args

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      args
    );

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtIn())
    | _               -> callExpr(f, newArgs, location=builtIn())
    end;

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(s.syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  forwards to
    foldStmt([
      setHeaderEntry,
      case fast, slow of
      | true,false  -> cilk_fastCloneSpawn(callF, nothingExpr())
      | false,true  -> cilk_slowCloneSpawn(callF, nothingExpr(), s.syncCount)
      | true,true   -> error ("We think we're in both a fast and a slow clone!")
      | false,false -> error ("We don't think we're in a fast or slow clone!")
      end
    ]);
}

abstract production cilk_fastCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr
{
  local beforeSpawnFast :: Stmt =
    txtStmt("Cilk_cilk2c_before_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));");

  local pushFrame :: Stmt = txtStmt("Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header));");

  local afterSpawnFast :: Stmt =
    foldStmt([
      txtStmt("Cilk_cilk2c_after_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
    ]);

  forwards to
    foldStmt([
      beforeSpawnFast,
      pushFrame,
      exprStmt(call),
      makeXPopFrame(ml),
      afterSpawnFast
    ]);
}

abstract production cilk_slowCloneSpawnWithEqOp
s::Stmt ::= l::Expr op::AssignOp callF::Expr syncCount::Integer
{
  -- TODO: assign to tmp, not l (is this done by RestoreVariables()?)
  -- l = callF();
  local assignExpr :: Expr =
    binaryOpExpr(
      l,
      assignOp(op, location=builtIn()),
      callF,
      location=builtIn()
    );

  forwards to cilk_slowCloneSpawn(assignExpr, justExpr(l), syncCount);
}

abstract production cilk_slowCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr syncCount::Integer
{
  -- expand CILK2C_BEFORE_SPAWN_SLOW() macro
  local beforeSpawnSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_BEFORE_SPAWN_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_before_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local pushFrame :: Stmt = txtStmt("Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header));");

  -- expand CILK2C_AFTER_SPAWN_SLOW() macro
  local afterSpawnSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AFTER_SPAWN_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_after_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local recoveryStmt :: Stmt = txtStmt("if (0) {_cilk_sync" ++ toString(syncCount) ++ ":;}");

  -- expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro
  local atThreadBoundary :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_at_thread_boundary_slow_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
    ]);

  -- TODO: set up link information
  forwards to
    foldStmt([
      beforeSpawnSlow,
      pushFrame,
      exprStmt(call),
      makeXPopFrame(nothingExpr()),
      afterSpawnSlow,
      recoveryStmt,
      atThreadBoundary
    ]);
}

{- based on cilkc2c/transform.c:MakeXPopFrame()

  forwards to:
  if (Cilk_cilk2c_pop_check(_cilk_ws)) {
    __tmp = result;
    if (Cilk_exception_handler(_cilk_ws, &__tmp, sizeof(__tmp))) {
      Cilk_cilk2c_pop(_cilk_ws);
      // r is
      //  /* nothing */ if slow or return void
      //  0 if return type is scalar
      //  _cilk_frame->dummy_return otherwise
      return r;
    }
  }

  OR if ml is nothingExpr()

  if (Cilk_cilk2c_pop_check(_cilk_ws)) {
    if (Cilk_exception_handler(_cilk_ws, (void *)0, 0)) {
      Cilk_cilk2c_pop(_cilk_ws);
      return;
    }
  }
-}
abstract production makeXPopFrame
top::Stmt ::= ml::MaybeExpr
{
  local l :: Expr =
    case ml of
    | justExpr(l1)  -> l1
    | nothingExpr() -> error("internal error, attempting to extract from nothingExpr()")
    end;
  l.env = top.env;

  local tmpName :: Name = name("__tmp", location=builtIn());
  local tmpDecl :: Stmt =
    declStmt(
      variableDecls([], [],
        directTypeExpr(l.typerep),
        foldDeclarator([
          declarator(
            tmpName,
            baseTypeExpr(),
            [],
            nothingInitializer()
          )
        ])
      )
    );

  local mTmpDecl :: Stmt =
    case ml of
    | justExpr(_)   -> tmpDecl
    | nothingExpr() -> nullStmt()
    end;

  local mAssignTmp :: Stmt =
    case ml of
    | justExpr(_)   -> assignTmp
    | nothingExpr() -> nullStmt()
    end;

  local ws :: Expr = declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn());

  local xPopFrameResult :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_cilk2c_pop_check", location=builtIn()),
        foldExpr([ws]),
        location=builtIn()
      ),
      foldStmt([
        mAssignTmp,
        ifExceptionHandler
      ])
    );

  local tmp :: Expr = declRefExpr(tmpName, location=builtIn());
  local assignTmp :: Stmt =
    exprStmt(
      binaryOpExpr(
        tmp,
        assignOp(eqOp(location=builtIn()), location=builtIn()),
        l,
        location=builtIn()
      )
    );

  local tmpAddr :: Expr =
    case ml of
    | justExpr(_)   -> mkAddressOf(tmp, builtIn())
    | nothingExpr() -> mkIntConst(0, builtIn())
    end;

  local sizeofTmp :: Expr =
    case ml of
    | justExpr(_) ->
        unaryExprOrTypeTraitExpr(
          sizeofOp(location=builtIn()),
          exprExpr(tmp),
          location=builtIn()
        )
    | nothingExpr() -> mkIntConst(0, builtIn())
    end;

  -- TODO: correct XPOP_FRAME_RESULT return
  --  /* nothing */ if slow or return void
  --  0 if return type is scalar
  --  _cilk_frame->dummy_return otherwise
  local retStmt :: Stmt =
    case ml of
    | justExpr(_)   -> txtStmt("return 0;")
    | nothingExpr() -> txtStmt("return;")
    end;

  local ifExceptionHandler :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_exception_handler", location=builtIn()),
        foldExpr([
          ws,
          tmpAddr,
          sizeofTmp
        ]),
        location=builtIn()
      ),
      foldStmt([
        txtStmt("Cilk_cilk2c_pop(_cilk_ws);"),
        retStmt
      ])
    );

  forwards to
    compoundStmt(
      foldStmt([
        mTmpDecl,
        xPopFrameResult
      ])
    );
}

-- _cilk_frame->header.entry = syncCount;
abstract production makeSetHeaderEntry
top::Stmt ::= syncCount::Integer
{
  forwards to
    exprStmt(
      binaryOpExpr(
        -- cilk_frame->header.entry
        memberExpr(
          -- cilk_frame->header
          memberExpr(
            declRefExpr(name("_cilk_frame", location=builtIn()), location=builtIn()),
            true,
            name("header", location=builtIn()),
            location=builtIn()
          ),
          false,
          name("entry", location=builtIn()),
          location=builtIn()
        ),
        assignOp(eqOp(location=builtIn()), location=builtIn()),
        mkIntConst(syncCount, builtIn()),
        location=builtIn()
      )
    );
}

-- return first found item; otherwise error
function lookupMiscString
String ::= n::String  e::Decorated Env
{
  local foundItems :: [MiscItem] = lookupMisc(n, e);
  local foundItem :: MiscItem =
    if   null(foundItems)
    then error(n ++ " not defined in Misc env")
    else head(foundItems);

  return
    case foundItem of
    | stringMiscItem(s) -> s
    | _                 -> error(n ++ " MiscItem is not a stringMiscItem")
    end;
}

