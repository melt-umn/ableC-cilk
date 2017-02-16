grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;
imports silver:util:raw:treemap as tm;

abstract production cilkSpawnStmt
s::Stmt ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), l.pp, space(), op.pp, space(),
                  f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs = [];
  s.freeVariables = [];
  s.functiondefs = [];

  s.scopeCount = s.scopeCountInh;
  s.scopes = s.scopesInh;

  -- TODO: transform args

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      args
    );

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(s.syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtIn())
    | _               -> callExpr(f, newArgs, location=builtIn())
    end;

  local spawnStmt :: Stmt =
    case fast,slow of
    | true,false  -> cilk_fastCloneSpawnWithEqOp(l, op, callF)
    | false,true  -> cilk_slowCloneSpawnWithEqOp(l, op, callF)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;

  forwards to
    foldStmt([
      setHeaderEntry,
      saveVariables(s.env),
      spawnStmt
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

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs = [];
  s.freeVariables = [];
  s.functiondefs = [];

  -- TODO: refactor this to reuse cilkSpawnStmt code

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

  local setHeaderEntry :: Stmt = makeSetHeaderEntry(s.syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local spawnStmt :: Stmt =
    case fast, slow of
    | true,false  -> cilk_fastCloneSpawn(callF, nothingExpr())
    | false,true  -> cilk_slowCloneSpawn(callF, nothingExpr())
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;

  forwards to
    compoundStmt(
      foldStmt([
        setHeaderEntry,
        saveVariables(s.env),
        spawnStmt
      ])
    );
}

abstract production cilk_fastCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr
{
  local beforeSpawnFast :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_BEFORE_SPAWN_FAST() macro */"),
      txtStmt("Cilk_cilk2c_before_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local pushFrame :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_PUSH_FRAME() macro */"),
      txtStmt("Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local afterSpawnFast :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AFTER_SPAWN_FAST() macro */"),
      txtStmt("Cilk_cilk2c_after_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
    ]);

  forwards to
    foldStmt([
      beforeSpawnFast,
      pushFrame,
      exprStmt(call),
      makeXPopFrame(ml, false),
      afterSpawnFast
    ]);
}

abstract production cilk_slowCloneSpawnWithEqOp
s::Stmt ::= l::Expr op::AssignOp callF::Expr
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

  forwards to cilk_slowCloneSpawn(assignExpr, justExpr(l));
}

abstract production cilk_slowCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr
{
  -- reserve a sync number
  s.syncCount = s.syncCountInh + 1;

  -- expand CILK2C_BEFORE_SPAWN_SLOW() macro
  local beforeSpawnSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_BEFORE_SPAWN_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_before_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local pushFrame :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_PUSH_FRAME() macro */"),
      txtStmt("Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header));")
    ]);

  -- expand CILK2C_AFTER_SPAWN_SLOW() macro
  local afterSpawnSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AFTER_SPAWN_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_after_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local recoveryStmt :: Stmt =
    ifStmtNoElse(
      mkIntConst(0, builtIn()),
      foldStmt([
        txtStmt("_cilk_sync" ++ toString(s.syncCount) ++ ":"),
        restoreVariables(s.env)
      ])
    );
    
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
      makeXPopFrame(ml, true),
      afterSpawnSlow,
      saveVariables(s.env),
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
top::Stmt ::= ml::MaybeExpr isSlow::Boolean
{
  local l :: Expr =
    case ml of
    | justExpr(l1)  -> l1
    | nothingExpr() -> error("internal error, attempting to extract from nothingExpr()")
    end;
  l.env = top.env;
  l.returnType = top.returnType;

  local tmpName :: Name = name("__tmp" ++ toString(genInt()), location=builtIn());
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
    | justExpr(_)   ->
        if isSlow
        then txtStmt("return;")
        else txtStmt("return 0;")
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
        txtStmt("/* expand CILK2C_XPOP_FRAME_RESULT() macro */"),
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

function saveVariables
Stmt ::= env::Decorated Env
{
  local values :: [ValueItem] =
    map (snd, foldr(append, [], map(tm:toList, take(length(env.values)-1, env.values))));
  local decls :: [Decorated Declarator] = getDeclsFromValues(values);
  return
    foldStmt([
      txtStmt("/* TODO: save only live, dirty variables */"),
      foldStmt(map(saveDecl, decls))
    ]);
}

function getDeclsFromValues
[Decorated Declarator] ::= values::[ValueItem]
{
  return
    case values of
    | v :: vs ->
        case v of
        | declaratorValueItem(s) -> s :: getDeclsFromValues(vs)
        | _                      -> getDeclsFromValues(vs)
        end
    | []         -> []
    end;
}

function saveDecl
Stmt ::= d::Decorated Declarator
{
  return 
    case d of
    | declarator(n, _, _, _) ->
        if n.name != "_cilk_frame"
        then
          txtStmt("_cilk_frame->scope" ++ toString(d.scopeCountInh) ++ "." ++
            n.name ++ " = " ++ n.name ++ ";")
        else
          nullStmt()
    | _                         ->
        txtStmt("/* unsupported declarator found, will not save */")
    end;
}

-- return first found item; otherwise error
--function lookupMiscString
--String ::= n::String  e::Decorated Env
--{
--  local foundItems :: [MiscItem] = lookupMisc(n, e);
--  local foundItem :: MiscItem =
--    if   null(foundItems)
--    then error(n ++ " not defined in Misc env")
--    else head(foundItems);
--
--  return
--    case foundItem of
--    | stringMiscItem(s) -> s
--    | _                 -> error(n ++ " MiscItem is not a stringMiscItem")
--    end;
--}

