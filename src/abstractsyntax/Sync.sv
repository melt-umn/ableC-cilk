grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

{- based on cilkc2c/transform.c:TransformSync() -}
abstract production cilk_syncStmt
s::Stmt ::= loc::Location
{
  s.pp = text("sync");

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs := [];
  s.freeVariables = [];
  s.functiondefs := [];

--  s.scopeCount = s.scopeCountInh;
--  s.cilkFrameDeclsScopes = s.cilkFrameDeclsScopesInh;
  s.cilkFrameDeclsScopes = [];

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneSync()
    | false,true  -> cilk_slowCloneSync(loc)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;
}

abstract production cilk_fastCloneSync
s::Stmt ::=
{
  -- expand CILK2C_AT_SYNC_FAST() macro
  forwards to
    foldStmt([
      txtStmt("/* expand CILK2C_AT_SYNC_FAST() macro */"),
      txtStmt("Cilk_cilk2c_at_sync_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
    ]);
}

abstract production cilk_slowCloneSync
s::Stmt ::= loc::Location
{
  -- reserve a sync number
--  s.syncCount = s.syncCountInh + 1;
  s.syncLocations = [loc];

  local foundSyncLocations :: [[Location]] = lookupSyncLocations(cilk_sync_locations_id, s.env);
  local allSyncLocations :: [Location] =
    if   null(foundSyncLocations)
    then error("syncLocations not passed down through environment")
    else head(foundSyncLocations);

  local syncCount :: Integer = positionOf(locationEq, loc, allSyncLocations) + 1;

  -- expand CILK2C_BEFORE_SYNC_SLOW() macro
  local beforeSyncSlow :: Stmt =
    foldStmt([
      txtStmt("/* syncCount: " ++ toString(syncCount) ++ " */"),
      txtStmt("/* expand CILK2C_BEFORE_SYNC_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_before_sync_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(loc);

  local recoveryStmt :: Stmt =
    ifStmtNoElse(
      -- expand CILK2C_SYNC macro to Cilk_sync(_cilk_ws)
      directCallExpr(
        name("Cilk_sync", location=builtIn()),
        foldExpr([
          declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn())
        ]),
        location=builtIn()
      ),
      foldStmt([
        txtStmt("return; _cilk_sync" ++ toString(makeSyncLabel(loc)) ++ ":;")
--        restoreVariables(s.env)
      ])
    );
    
  -- expand CILK2C_AFTER_SYNC_SLOW() macro
  local afterSyncSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AFTER_SYNC_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_after_sync_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  -- expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro
  local atThreadBoundary :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AT_THREAD_BOUNDARY_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_at_thread_boundary_slow_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
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

function makeSyncLabel
Integer ::= loc::Location
{
  -- TODO: possible problem if line has more than 10000 characters?
  return 10000 * loc.line + loc.column;
}

