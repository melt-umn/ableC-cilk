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
  s.defs := [];
  s.freeVariables = [];
  s.functiondefs := [];

  s.cilkFrameDeclsScopes = [];

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=bogusLoc()), location=bogusLoc()),
      args
    );

  local syncCount :: Integer = lookupSyncCount(l.location, s.env);

  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=bogusLoc())
    | _               -> callExpr(f, newArgs, location=bogusLoc())
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
      assignOp(op, location=bogusLoc()),
      callF,
      location=bogusLoc()
    );

  forwards to cilk_fastCloneSpawn(assignExpr, justExpr(l), l.location);
}

abstract production cilkSpawnStmtNoEqOp
s::Stmt ::= f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs := [];
  s.freeVariables = [];
  s.functiondefs := [];

  s.cilkFrameDeclsScopes = [];

  -- TODO: refactor this to reuse cilkSpawnStmt code

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=bogusLoc()), location=bogusLoc()),
      args
    );

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=bogusLoc())
    | _               -> callExpr(f, newArgs, location=bogusLoc())
    end;

  local syncCount :: Integer = lookupSyncCount(f.location, s.env);

  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local spawnStmt :: Stmt =
    case fast, slow of
    | true,false  -> cilk_fastCloneSpawn(callF, nothingExpr(), f.location)
    | false,true  -> cilk_slowCloneSpawn(callF, nothingExpr(), nullStmt(), f.location)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;

  s.cilkLinks =
    case fast, slow of
    | true,false  -> s.cilkLinksInh
    | false,true  ->
         cons(
           init(objectInitializer(
             foldInit([
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc())))
             ])
           )),
           s.cilkLinksInh
         )
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
s::Stmt ::= call::Expr ml::MaybeExpr loc::Location
{
  -- reserve a sync number
  s.syncLocations = [loc];

  local syncCount :: Integer = lookupSyncCount(loc, s.env);

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
  local lIsGlobal :: Boolean =
    !containsBy(
      stringEq, lName.name,
      map(fst, foldr(append, [], map(tm:toList, take(length(s.env.scopeIds)-1, s.env.scopeIds))))
     );

  s.cilkLinks =
    if   lIsGlobal
    then s.cilkLinksInh
    else
         cons(
           init(objectInitializer(
             foldInit([
               init(exprInitializer(sizeofL)),
               init(exprInitializer(frameOffset)),
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc()))),
               init(exprInitializer(mkIntConst(0, bogusLoc())))
             ])
           )),
           s.cilkLinksInh
         );

  local sizeofL :: Expr =
    unaryExprOrTypeTraitExpr(
      sizeofOp(location=bogusLoc()),
      typeNameExpr(typeName(directTypeExpr(l.typerep), baseTypeExpr())),
      location=bogusLoc()
    );

  -- FIXME: l should be an id, not an Expr
  local lName :: Name =
    case l of
    | declRefExpr(id) -> id
    | _               -> error("spawn lhs must be an id")
    end;

  -- TODO: check that lookupScopeId does not return Nil
  local lScopeId :: String = head(lookupScopeId(lName.name, s.env));
  local scopeName :: Name = name("scope" ++ lScopeId, location=bogusLoc());
  local frameName :: Name = name("_cilk_" ++ s.cilkProcName.name ++ "_frame", location=bogusLoc());

  local saveL :: Stmt =
    if   lIsGlobal
    then nullStmt()
    else
         txtStmt("_cilk_frame->" ++ scopeName.name ++ "." ++ lName.name ++ " = " ++ lName.name ++ ";");

  local frameTypeExpr :: BaseTypeExpr =
    tagReferenceTypeExpr([], structSEU(), frameName);

  -- expand CILK_OFFSETOF(struct _cilk_func_frame, scopeX.l) to
  -- ((size_t) ((char *)&((struct _cilk_func_frame *) 0)->scopeX.l - (char *)((struct _cilk_func_frame *) 0)))
  local frameOffset :: Expr =
    -- TODO: don't use txtExpr for frameOffset
    txtExpr(
      "((size_t) ((char *)&((struct " ++ frameName.name ++ " *) 0)->"
        ++ scopeName.name ++ "." ++ lName.name ++ " - (char *)((struct " ++ frameName.name
        ++ " *) 0)))",
      location=bogusLoc()
    );

  -- l = callF();
  local assignExpr :: Expr =
    binaryOpExpr(
      l,
      assignOp(op, location=bogusLoc()),
      callF,
      location=bogusLoc()
    );

  forwards to cilk_slowCloneSpawn(assignExpr, justExpr(l), saveL, l.location);
}

abstract production cilk_slowCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr saveAssignedVar::Stmt loc::Location
{
  -- reserve a sync number
  s.syncLocations = [loc];

  local syncCount :: Integer = lookupSyncCount(loc, s.env);

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
      mkIntConst(0, bogusLoc()),
      foldStmt([
        txtStmt("_cilk_sync" ++ toString(syncCount) ++ ":;"),
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

  forwards to
    foldStmt([
      beforeSpawnSlow,
      pushFrame,
      exprStmt(call),

      saveAssignedVar,
      makeXPopFrame(ml, true),
      restoreVariables(s.env),

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

  local tmpName :: Name = name("__tmp" ++ toString(genInt()), location=bogusLoc());
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

  local ws :: Expr = declRefExpr(name("_cilk_ws", location=bogusLoc()), location=bogusLoc());

  local xPopFrameResult :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_cilk2c_pop_check", location=bogusLoc()),
        foldExpr([ws]),
        location=bogusLoc()
      ),
      foldStmt([
        mAssignTmp,
        ifExceptionHandler
      ])
    );

  local tmp :: Expr = declRefExpr(tmpName, location=bogusLoc());
  local assignTmp :: Stmt =
    exprStmt(
      binaryOpExpr(
        tmp,
        assignOp(eqOp(location=bogusLoc()), location=bogusLoc()),
        l,
        location=bogusLoc()
      )
    );

  local tmpAddr :: Expr =
    case ml of
    | justExpr(_)   -> mkAddressOf(tmp, bogusLoc())
    | nothingExpr() -> mkIntConst(0, bogusLoc())
    end;

  local sizeofTmp :: Expr =
    case ml of
    | justExpr(_) ->
        unaryExprOrTypeTraitExpr(
          sizeofOp(location=bogusLoc()),
          exprExpr(tmp),
          location=bogusLoc()
        )
    | nothingExpr() -> mkIntConst(0, bogusLoc())
    end;

  -- TODO: correct XPOP_FRAME_RESULT return
  --  /* nothing */ if slow or return void
  --  0 if return type is scalar
  --  _cilk_frame->dummy_return otherwise
  local retStmt :: Stmt =
    case ml of
    | justExpr(_)   ->
--        if isSlow || returnsVoid
        if isSlow
        then txtStmt("return;")
        else txtStmt("return 0;")
    | nothingExpr() -> txtStmt("return;")
    end;

  local ifExceptionHandler :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_exception_handler", location=bogusLoc()),
        foldExpr([
          ws,
          tmpAddr,
          sizeofTmp
        ]),
        location=bogusLoc()
      ),
      foldStmt([
        txtStmt("Cilk_cilk2c_pop(_cilk_ws);"),
        retStmt
      ])
    );

  local expandComment :: Stmt =
    case ml of
    | justExpr(_)   -> txtStmt("/* expand CILK2C_XPOP_FRAME_RESULT() macro */")
    | nothingExpr() -> txtStmt("/* expand CILK2C_XPOP_FRAME_NORESULT() macro */")
    end;

  forwards to
    compoundStmt(
      foldStmt([
        expandComment,
        mTmpDecl,
        xPopFrameResult
      ])
    );
}

-- _cilk_frame->header.entry = syncCount;
function makeSetHeaderEntry
Stmt ::= syncCount::Integer
{
  return
    exprStmt(
      binaryOpExpr(
        -- cilk_frame->header.entry
        memberExpr(
          -- cilk_frame->header
          memberExpr(
            declRefExpr(name("_cilk_frame", location=bogusLoc()), location=bogusLoc()),
            true,
            name("header", location=bogusLoc()),
            location=bogusLoc()
          ),
          false,
          name("entry", location=bogusLoc()),
          location=bogusLoc()
        ),
        assignOp(eqOp(location=bogusLoc()), location=bogusLoc()),
        mkIntConst(syncCount, bogusLoc()),
        location=bogusLoc()
      )
    );
}

function locationEq
Boolean ::= l1::Location l2::Location
{
  return l1.filename == l2.filename && l1.line == l2.line && l1.column == l2.column;
}

function lookupSyncCount
Integer ::= loc::Location  env::Decorated Env
{
  local foundSyncLocations :: [[Location]] = lookupSyncLocations(cilk_sync_locations_id, env);
  local allSyncLocations :: [Location] =
    if   null(foundSyncLocations)
    then error("syncLocations not passed down through environment")
    else head(foundSyncLocations);

  return positionOf(locationEq, loc, allSyncLocations) + 1;
}

