grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

{- based on cilkc2c/transform.c:TransformSync() -}
abstract production cilk_syncStmt
s::Stmt ::=
{
  s.pp = text("sync");

  -- s.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  s.globalDecls := [];
  s.defs = [];
  s.freeVariables = [];
  s.functiondefs = [];

  s.syncCount = s.syncCountInh + 1;

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneSync()
    | false,true  -> cilk_slowCloneSync(s.syncCount)
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
s::Stmt ::= syncCount::Integer
{
  -- expand CILK2C_BEFORE_SYNC_SLOW() macro
  local beforeSyncSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_BEFORE_SYNC_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_before_sync_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt = makeSetHeaderEntry(syncCount);

  -- TODO: save live, dirty variables

  -- expand CILK2C_AFTER_SYNC_SLOW() macro
  local afterSyncSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_AFTER_SYNC_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_after_sync_slow_cp(_cilk_ws, &(_cilk_frame->header));")
    ]);

  -- TODO: restore variables
  local syncLabel :: Stmt =
    -- note: expand CILK2C_SYNC macro to Cilk_sync(_cilk_ws)
    txtStmt("if (Cilk_sync(_cilk_ws)) {return; _cilk_sync" ++ toString(syncCount) ++ ":;}");

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
      syncLabel,
      afterSyncSlow,
      atThreadBoundary
    ]);
}

autocopy attribute syncCountInh :: Integer occurs on Stmt, Expr;
synthesized attribute syncCount :: Integer occurs on Stmt, Expr;

aspect production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
}

aspect production nestedFunctionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
}

aspect default production
top::Stmt ::=
{
  top.syncCount = top.syncCountInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  t.syncCountInh = h.syncCount;
  top.syncCount = t.syncCount;
}

aspect production exprStmt
top::Stmt ::= d::Expr
{
  top.syncCount = d.syncCount;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
}

-- we assume there are no syncs in most forms of expressions

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  e.syncCountInh = t.syncCount;
  top.syncCount = e.syncCount;
}

aspect production ifStmtNoElse
top::Stmt ::= c::Expr t::Stmt
{
  top.syncCount = t.syncCount;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncCount = b.syncCount;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect default production
top::Expr ::=
{
  top.syncCount = top.syncCountInh;
}

aspect production parenExpr
top::Expr ::= e::Expr
{
  top.syncCount = e.syncCount;
}

aspect production stmtExpr
top::Expr ::= body::Stmt result::Expr
{
  result.syncCountInh = body.syncCount;
  top.syncCount = result.syncCount;
}

