grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;
imports silver:util:treemap as tm;

abstract production cilkSpawnStmt
s::Stmt ::= l::Expr f::Expr args::Exprs
{
  s.pp = ppConcat([ text("spawn"), space(), l.pp, space(), text("="), space(),
                  f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  s.functionDefs := [];
  s.labelDefs := [];

  s.cilkFrameDeclsScopes = [];

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      args
    );

  local syncCount :: Integer = lookupSyncCount(l.location, s.env);

  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtinLoc(MODULE_NAME))
    | _               -> callExpr(f, newArgs, location=builtinLoc(MODULE_NAME))
    end;

  s.errors := case fast,slow of
    | true,false  -> forward.errors
    | false,true  -> forward.errors
    | true,true   -> []
    | false,false -> []
    end;

  local spawnStmt :: Stmt =
    case fast,slow of
    | true,false  -> cilk_fastCloneSpawnWithEqOp(l, callF)
    | false,true  -> cilk_slowCloneSpawnWithEqOp(l, callF)
    | true,true   -> error ("We think we're in both a fast and a slow clone!1")
    | false,false -> exprStmt(eqExpr(l, callF, location=builtinLoc(MODULE_NAME)))
    end;

  -- this causes sync to fail because lookupMisc(cilk_in_fast_clone) fails
  --s.errors := [];

  -- this raise error the 'count_matches' is not declared
  --s.errors := f.errors ++ args.errors ;

  -- not defining s.errors computes it on forwards-to tree and results
  -- in same error as above when we collect errors from f and args
  
  forwards to
    foldStmt([
      setHeaderEntry,
      saveVariables(s.env),
      spawnStmt
    ]);
}

abstract production cilk_fastCloneSpawnWithEqOp
s::Stmt ::= l::Expr callF::Expr
{
  --s.errors := [] ; -- TODO .... l.type   ++ f.erros ++ args.errors ;

  -- l = callF();
  -- TODO: handle assignOps other than eq
  local assignExpr :: Expr = eqExpr(l, callF, location=builtinLoc(MODULE_NAME));

  forwards to cilk_fastCloneSpawn(assignExpr, justExpr(l), l.location);
}

abstract production cilkSpawnStmtNoEqOp
s::Stmt ::= f::Expr args::Exprs
{
  s.pp = ppConcat([ text("spawn"), space(), f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  s.functionDefs := [];
  s.labelDefs := [];

  s.cilkFrameDeclsScopes = [];

  -- TODO: refactor this to reuse cilkSpawnStmt code

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      args
    );

  local callF :: Expr =
    case f of
    | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtinLoc(MODULE_NAME))
    | _               -> callExpr(f, newArgs, location=builtinLoc(MODULE_NAME))
    end;

  local syncCount :: Integer = lookupSyncCount(f.location, s.env);

  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  s.errors := case fast,slow of
    | true,false  -> forward.errors
    | false,true  -> forward.errors
    | true,true   -> []
    | false,false -> []
    end;

  local spawnStmt :: Stmt =
    case fast, slow of
    | true,false  -> cilk_fastCloneSpawn(callF, nothingExpr(), f.location)
    | false,true  -> cilk_slowCloneSpawn(callF, nothingExpr(), nullStmt(), f.location)
    | true,true   -> error ("We think we're in both a fast and a slow clone!3")
--    | false,false -> error ("We don't think we're in a fast or slow clone!4")
    | false,false -> exprStmt(callF)
    end;

  s.cilkLinks =
    case fast, slow of
    | true,false  -> s.cilkLinksInh
    | false,true  ->
         cons(
           positionalInit(objectInitializer(
             foldInit([
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)))
             ]),
             location=builtinLoc(MODULE_NAME)
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
      exprStmt(comment("expand CILK2C_BEFORE_SPAWN_FAST() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_before_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  local pushFrame :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_PUSH_FRAME() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  local afterSpawnFast :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_AFTER_SPAWN_FAST() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_after_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header)); },
      ableC_Stmt { Cilk_cilk2c_event_new_thread_maybe(_cilk_ws); }
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
s::Stmt ::= l::Expr callF::Expr
{
  s.pp = ppConcat([ text("spawn"), space(), l.pp, space(), text("="), space(), callF.pp]);
  s.functionDefs := [];
  s.labelDefs := [];

  local lIsGlobal :: Boolean =
    !contains(lName.name, map(fst, foldr(append, [], map(tm:toList, take(length(s.env.scopeIds)-1, s.env.scopeIds)))));

  s.cilkLinks =
    if   lIsGlobal
    then s.cilkLinksInh
    else
         cons(
           positionalInit(objectInitializer(
             foldInit([
               positionalInit(exprInitializer(sizeofL, location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(frameOffset, location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
               positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)))
             ]),
             location=builtinLoc(MODULE_NAME)
           )),
           s.cilkLinksInh
         );

  local sizeofL :: Expr =
    sizeofExpr(
      typeNameExpr(typeName(directTypeExpr(l.typerep), baseTypeExpr())),
      location=builtinLoc(MODULE_NAME)
    );

  -- FIXME: l should be an id, not an Expr
  local lName :: Name =
    case l of
    | declRefExpr(id) -> id
    | _               -> error("spawn lhs must be an id")
    end;

  -- TODO: check that lookupScopeId does not return Nil
  local lScopeId :: String = head(lookupScopeId(lName.name, s.env));
  local scopeName :: Name = name("scope" ++ lScopeId, location=builtinLoc(MODULE_NAME));
  local frameName :: Name = name("_cilk_" ++ s.cilkProcName.name ++ "_frame", location=builtinLoc(MODULE_NAME));

  local saveL :: Stmt =
    if   lIsGlobal
    then nullStmt()
    else ableC_Stmt { _cilk_frame->$Name{scopeName}.$Name{lName} = $Name{lName}; };

  local frameTypeExpr :: BaseTypeExpr =
    tagReferenceTypeExpr(nilQualifier(), structSEU(), frameName);

  -- expand CILK_OFFSETOF(struct _cilk_func_frame, scopeX.l) to
  -- ((size_t) ((char *)&((struct _cilk_func_frame *) 0)->scopeX.l - (char *)((struct _cilk_func_frame *) 0)))
  local frameOffset :: Expr =
    explicitCastExpr(
      typeName(
        typedefTypeExpr(nilQualifier(), name("size_t", location=builtinLoc(MODULE_NAME))),
        baseTypeExpr()
      ),
      subExpr(
        explicitCastExpr(
          typeName(
            builtinTypeExpr(nilQualifier(), signedType(charType())),
            pointerTypeExpr(nilQualifier(), baseTypeExpr())
          ),
          mkAddressOf(
            memberExpr(
              memberExpr(
                explicitCastExpr(
                  typeName(
                    tagReferenceTypeExpr(nilQualifier(), structSEU(), frameName),
                    pointerTypeExpr(nilQualifier(), baseTypeExpr())
                  ),
                  mkIntConst(0, builtinLoc(MODULE_NAME)),
                  location=builtinLoc(MODULE_NAME)
                ),
                true,
                scopeName,
                location=builtinLoc(MODULE_NAME)
              ),
              false,
              lName,
              location=builtinLoc(MODULE_NAME)
            ),
            builtinLoc(MODULE_NAME)
          ),
          location=builtinLoc(MODULE_NAME)
        ),
        explicitCastExpr(
          typeName(
            builtinTypeExpr(nilQualifier(), signedType(charType())),
            pointerTypeExpr(nilQualifier(), baseTypeExpr())
          ),
          explicitCastExpr(
            typeName(
              tagReferenceTypeExpr(nilQualifier(), structSEU(), frameName),
              pointerTypeExpr(nilQualifier(), baseTypeExpr())
            ),
            mkIntConst(0, builtinLoc(MODULE_NAME)),
            location=builtinLoc(MODULE_NAME)
          ),
          location=builtinLoc(MODULE_NAME)
        ),
        location=builtinLoc(MODULE_NAME)
      ),
      location=builtinLoc(MODULE_NAME)
    );

  -- l = callF();
  local assignExpr :: Expr = eqExpr(l, callF, location=builtinLoc(MODULE_NAME));

  forwards to cilk_slowCloneSpawn(assignExpr, justExpr(l), saveL, l.location);
}

abstract production cilk_slowCloneSpawn
s::Stmt ::= call::Expr ml::MaybeExpr saveAssignedVar::Stmt loc::Location
{
  s.pp = ppConcat([ text("spawn"), space(), call.pp ]);
  s.functionDefs := [];
  s.labelDefs := [];

  -- reserve a sync number
  s.syncLocations = [loc];

  local syncCount :: Integer = lookupSyncCount(loc, s.env);

  -- expand CILK2C_BEFORE_SPAWN_SLOW() macro
  local beforeSpawnSlow :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_BEFORE_SPAWN_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_before_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  local pushFrame :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_PUSH_FRAME() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  -- expand CILK2C_AFTER_SPAWN_SLOW() macro
  local afterSpawnSlow :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_AFTER_SPAWN_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_after_spawn_slow_cp(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  local recoveryStmt :: Stmt =
    ifStmtNoElse(
      mkIntConst(0, builtinLoc(MODULE_NAME)),
      foldStmt([
        -- TODO: in the long term, txtStmt should be replaced with labelStmt,
        --       but there are issues with functiondefs being overridden in
        --       cilkSpawnStmt, see ableC Issue #77 for more info
        --       https://github.com/melt-umn/ableC/issues/77
        txtStmt("_cilk_sync" ++ toString(syncCount) ++ ":;"),
--        labelStmt(name("_cilk_sync" ++ toString(syncCount), location=builtinLoc(MODULE_NAME)), nullStmt()),
        restoreVariables(s.env)
      ])
    );
    
  -- expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro
  local atThreadBoundary :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_at_thread_boundary_slow_cp(_cilk_ws, &(_cilk_frame->header)); },
      ableC_Stmt { Cilk_cilk2c_event_new_thread_maybe(_cilk_ws); }
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
  top.pp = text("cilkMakeXPopFrame()"); -- TODO: better pp
  top.functionDefs := [];
  top.labelDefs := [];

  local l :: Expr =
    case ml of
    | justExpr(l1)  -> l1
    | nothingExpr() -> error("internal error, attempting to extract from nothingExpr()")
    end;
  l.env = top.env;
  l.controlStmtContext = top.controlStmtContext;

  local tmpName :: Name = name("__tmp" ++ toString(genInt()), location=builtinLoc(MODULE_NAME));
  local tmpDecl :: Stmt =
    declStmt(
      variableDecls(nilStorageClass(), nilAttribute(),
        directTypeExpr(l.typerep),
        foldDeclarator([
          declarator(
            tmpName,
            baseTypeExpr(),
            nilAttribute(),
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

  local ws :: Expr = declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME));

  local xPopFrameResult :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_cilk2c_pop_check", location=builtinLoc(MODULE_NAME)),
        foldExpr([ws]),
        location=builtinLoc(MODULE_NAME)
      ),
      foldStmt([
        mAssignTmp,
        ifExceptionHandler
      ])
    );

  local tmp :: Expr = declRefExpr(tmpName, location=builtinLoc(MODULE_NAME));
  local assignTmp :: Stmt = exprStmt(eqExpr( tmp, l, location=builtinLoc(MODULE_NAME)));

  local tmpAddr :: Expr =
    case ml of
    | justExpr(_)   -> mkAddressOf(tmp, builtinLoc(MODULE_NAME))
    | nothingExpr() -> mkIntConst(0, builtinLoc(MODULE_NAME))
    end;

  local sizeofTmp :: Expr =
    case ml of
    | justExpr(_) ->
        sizeofExpr(
          exprExpr(tmp),
          location=builtinLoc(MODULE_NAME)
        )
    | nothingExpr() -> mkIntConst(0, builtinLoc(MODULE_NAME))
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
        then ableC_Stmt { return; }
        else ableC_Stmt { return 0; }
    | nothingExpr() -> ableC_Stmt { return; }
    end;

  local ifExceptionHandler :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_exception_handler", location=builtinLoc(MODULE_NAME)),
        foldExpr([
          ws,
          tmpAddr,
          sizeofTmp
        ]),
        location=builtinLoc(MODULE_NAME)
      ),
      foldStmt([
        ableC_Stmt { Cilk_cilk2c_pop(_cilk_ws); },
        retStmt
      ])
    );

  local expandComment :: Stmt =
    case ml of
    | justExpr(_)   -> exprStmt(comment("expand CILK2C_XPOP_FRAME_RESULT() macro", location=builtinLoc(MODULE_NAME)))
    | nothingExpr() -> exprStmt(comment("expand CILK2C_XPOP_FRAME_NORESULT() macro", location=builtinLoc(MODULE_NAME)))
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
      eqExpr(
        -- cilk_frame->header.entry
        memberExpr(
          -- cilk_frame->header
          memberExpr(
            declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
            true,
            name("header", location=builtinLoc(MODULE_NAME)),
            location=builtinLoc(MODULE_NAME)
          ),
          false,
          name("entry", location=builtinLoc(MODULE_NAME)),
          location=builtinLoc(MODULE_NAME)
        ),
        mkIntConst(syncCount, builtinLoc(MODULE_NAME)),
        location=builtinLoc(MODULE_NAME)
      )
    );
}

function lookupSyncCount
Integer ::= loc::Location  env::Decorated Env
{
  local foundSyncLocations :: [[Location]] = lookupSyncLocations(cilk_sync_locations_id, env);
  local allSyncLocations :: [Location] =
    if   null(foundSyncLocations)
    then error("syncLocations not passed down through environment")
    else head(foundSyncLocations);

  return positionOf(loc, allSyncLocations) + 1;
}

