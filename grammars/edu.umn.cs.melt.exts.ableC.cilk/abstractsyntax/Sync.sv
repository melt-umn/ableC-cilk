grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;


{- based on cilkc2c/transform.c:TransformSync() -}
abstract production cilk_syncStmt
s::Stmt ::= loc::Location
{
  s.pp = text("sync");

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs := [];
  s.freeVariables := [];
  s.functionDefs := [];
  s.labelDefs := [];

  s.cilkFrameDeclsScopes = [];

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  s.errors := case fast,slow of
    | true,false  -> forward.errors
    | false,true  -> forward.errors
    | true,true   -> []
    | false,false -> []
    end;

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneSync(loc)
    | false,true  -> cilk_slowCloneSync(loc)
    | true,true   -> error ("We think we're in both a fast and a slow clone!5")
    | false,false -> nullStmt()
    end;
}

abstract production cilk_fastCloneSync
s::Stmt ::= loc::Location
{
  -- reserve a sync number
  s.syncLocations = [loc];

  -- expand CILK2C_AT_SYNC_FAST() macro
  forwards to
    foldStmt([
      exprStmt(comment("expand CILK2C_AT_SYNC_FAST() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_at_sync_fast_cp(_cilk_ws, &(_cilk_frame->header)); },
      ableC_Stmt { Cilk_cilk2c_event_new_thread_maybe(_cilk_ws); }
    ]);
}

abstract production cilk_slowCloneSync
s::Stmt ::= loc::Location
{
  s.pp = text("sync");
  s.functionDefs := [];
  s.labelDefs := [];
  -- reserve a sync number
  s.syncLocations = [loc];

  local syncCount :: Integer = lookupSyncCount(loc, s.env);

  s.cilkLinks =
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
     );

  -- expand CILK2C_BEFORE_SYNC_SLOW() macro
  local beforeSyncSlow :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_BEFORE_SYNC_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_before_sync_slow_cp(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  local recoveryStmt :: Stmt =
    ifStmtNoElse(
      -- expand CILK2C_SYNC macro to Cilk_sync(_cilk_ws)
      directCallExpr(
        name("Cilk_sync", location=builtinLoc(MODULE_NAME)),
        foldExpr([
          declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))
        ]),
        location=builtinLoc(MODULE_NAME)
      ),
      foldStmt([
        ableC_Stmt { return; },
        txtStmt("_cilk_sync" ++ toString(syncCount) ++ ":;")
        -- TODO: replace txtStmt with labelStmt
--        labelStmt(name("_cilk_sync" ++ toString(syncCount), location=builtinLoc(MODULE_NAME)), nullStmt()),
--        restoreVariables(s.env)
      ])
    );
    
  -- expand CILK2C_AFTER_SYNC_SLOW() macro
  local afterSyncSlow :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_AFTER_SYNC_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_after_sync_slow_cp(_cilk_ws, &(_cilk_frame->header)); }
    ]);

  -- expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro
  local atThreadBoundary :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro", location=builtinLoc(MODULE_NAME))),
      ableC_Stmt { Cilk_cilk2c_at_thread_boundary_slow_cp(_cilk_ws, &(_cilk_frame->header)); },
      ableC_Stmt { Cilk_cilk2c_event_new_thread_maybe(_cilk_ws); }
    ]);

  forwards to
    foldStmt([
      beforeSyncSlow,
      setHeaderEntry,
      saveVariables(s.env),
      recoveryStmt,
      restoreVariables(s.env), -- TODO: should this be here?
      afterSyncSlow,
      atThreadBoundary
    ]);
}

