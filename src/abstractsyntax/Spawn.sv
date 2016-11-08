grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilkSpawnStmt
s::Stmt ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), l.pp, space(), op.pp, space(),
                  f.pp, parens( ppImplode(text(","), args.pps) ) ]);

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, s.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, s.env));

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneSpawn(l, op, f, args)
    | false,true  -> cilk_slowCloneSpawn(l, op, f, args)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;
}

abstract production cilk_fastCloneSpawn
s::Stmt ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  s.syncCount = s.syncCountInh + 1;

  -- add _cilk_ws as first argument
  local newArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      args
    );

  --s.errors := [] ; -- TODO .... l.type   ++ f.erros ++ args.errors ;
  local assignExpr :: Expr =
    binaryOpExpr(
      l,
      assignOp(op, location=builtIn()),
      case f of
      | declRefExpr(id) -> directCallExpr(id, newArgs, location=builtIn())
      | _               -> callExpr(f, newArgs, location=builtIn())
      end,
      location=builtIn()
    );

  -- _cilk_frame->header.entry = syncCount;
  local setHeaderEntry :: Stmt =
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
        assignOp(op, location=builtIn()),
        mkIntConst(s.syncCount, builtIn()),
        location=builtIn()
      )
    );

  local beforeSpawnFast :: Stmt =
    txtStmt("Cilk_cilk2c_before_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));");

  local pushFrame :: Stmt =
    txtStmt("Cilk_cilk2c_push_frame(_cilk_ws, &(_cilk_frame->header));");

  local afterSpawnFast :: Stmt =
    foldStmt([
      txtStmt("Cilk_cilk2c_after_spawn_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_event_new_thread_maybe(_cilk_ws);")
    ]);

  forwards to
    foldStmt([
      setHeaderEntry,
      beforeSpawnFast,
      pushFrame,
      exprStmt(assignExpr),
      makeXPopFrame(l),
      afterSpawnFast
    ]);
}

abstract production cilk_slowCloneSpawn
s::Stmt ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  forwards to error("cilk_slowCloneSpawn() not implemented yet");
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
-}
abstract production makeXPopFrame
top::Stmt ::= l::Expr
{
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

  local ws :: Expr = declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn());

  local xPopFrameResult :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_cilk2c_pop_check", location=builtIn()),
        foldExpr([ws]),
        location=builtIn()
      ),
      foldStmt([
        assignTmp,
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

  local ifExceptionHandler :: Stmt =
    ifStmtNoElse(
      directCallExpr(
        name("Cilk_exception_handler", location=builtIn()),
        foldExpr([
          ws,
          mkAddressOf(tmp, builtIn()),
          unaryExprOrTypeTraitExpr(
            sizeofOp(location=builtIn()),
            exprExpr(tmp),
            location=builtIn()
          )
        ]),
        location=builtIn()
      ),
      foldStmt([
        txtStmt("Cilk_cilk2c_pop(_cilk_ws);"),

        -- TODO: correct XPOP_FRAME_RESULT return
        --  /* nothing */ if slow or return void
        --  0 if return type is scalar
        --  _cilk_frame->dummy_return otherwise
        txtStmt("return 0;")
      ])
    );

  forwards to
    compoundStmt(
      foldStmt([
        tmpDecl,
        xPopFrameResult
      ])
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

