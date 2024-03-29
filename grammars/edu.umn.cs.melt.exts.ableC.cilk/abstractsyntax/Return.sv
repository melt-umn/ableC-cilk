grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

abstract production cilk_returnStmt
r::Stmt ::= e::MaybeExpr
{
  r.pp = ppConcat ([ text("cilk"), space(), text("return"), space(), parens(e.pp) ]);

  propagate controlStmtContext, env;

  -- r.env depends on these, if not set then compiler will crash while looping
  --  in forwarded returnStmt to look for these
  r.globalDecls := e.globalDecls;
  r.defs := e.defs;
  r.functionDefs := [];
  r.labelDefs := [];

  r.cilkFrameDeclsScopes = [];

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, r.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, r.env));

  r.errors := case fast,slow of
    | true,false  -> forward.errors
    | false,true  -> forward.errors
    | true,true   -> []
    | false,false -> []
    end;

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneReturn(e)
    | false,true  -> cilk_slowCloneReturn(e)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> returnStmt(e)
    end;
}

{- A return in a fast clone stores the result to return in a temporary,
   calls CILK2C_BEFORE_RETURN_FAST();, then returns the temp.
 -}
abstract production cilk_fastCloneReturn
r::Stmt ::= e::MaybeExpr
{
  local tempInt::Integer = genInt();

  r.pp = pp"cilk return ${e.pp}";
  r.functionDefs := [];
  r.labelDefs := [];

  -- TODO: check if needs_sync? (see cilk2c/transform.c:TransformReturn())

  -- expand CILK2C_START_THREAD_FAST() macro
  local beforeReturnFast :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_BEFORE_RETURN_FAST() macro")),
      -- Cilk_cilk2c_before_return_fast_cp(_cilk_ws, &(_cilk_frame->header));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_before_return_fast_cp"),
          foldExpr([
            declRefExpr(name("_cilk_ws")),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame")),
                true,
                name("header")
              ),
              builtinLoc(MODULE_NAME)
            )
          ])
        )
      ),
      -- Cilk_cilk2c_before_return_fast(_cilk_ws, &(_cilk_frame->header), sizeof(*_cilk_frame));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_before_return_fast"),
          foldExpr([
            declRefExpr(name("_cilk_ws")),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame")),
                true,
                name("header")
              ),
              builtinLoc(MODULE_NAME)
            ),
            sizeofExpr(
              exprExpr(
                dereferenceExpr(
                  declRefExpr(name("_cilk_frame"))
                )
              )
            )
          ])
        )
      )
    ]);

  local returnType :: Type =
    case r.controlStmtContext.returnType of
    | just(ty)  -> ty
    | nothing() -> error("returnType is required by cilk return")
    end;

  -- ToDo: extract return type of the function from env and use below.
  -- Now we assume the return type is int.
  -- or use gcc type-of thing.
  forwards to
    compoundStmt(
      foldStmt(
        case e.justTheExpr of
        | just(e) ->
            [
              declStmt(
                variableDecls(nilStorageClass(), nilAttribute(),
                  returnType.baseTypeExpr,
                  foldDeclarator([
                    declarator(
                      -- TODO: cilk2c numbers tmps (e.g. _cilk_temp0), is this necessary?
                      name("_cilk_tmp"),
                      returnType.typeModifierExpr,
                      nilAttribute(),
                      justInitializer(exprInitializer(e))
                    )
                  ])
                )
              ),
              beforeReturnFast,
              returnStmt(
                justExpr(
                  declRefExpr(name("_cilk_tmp"))
                )
              )
            ]
        | nothing() ->
           [ beforeReturnFast,
             returnStmt(nothingExpr()) ]
        end
      )
    );
}

{- A return in a slow clone stores the result to return in a
   temporary, calls Cilk_set_result to store this in the frame,
   calls CILK2C_BEFORE_RETURN_SLOW(), then returns nothing.  Recall
   that the return type of the slow clone is void.
 -}
abstract production cilk_slowCloneReturn
r::Stmt ::= me::MaybeExpr
{
  r.pp = ppConcat([text("cilk return"), space(), me.pp, semi()]);
  r.functionDefs := [];
  r.labelDefs := [];

  local e :: Expr =
    case me of
      justExpr(e1)  -> e1
    | nothingExpr() -> error("internal error in cilk_slowCloneReturn, attempting to extract from nothingExpr()")
    end;
  e.env = r.env;
  e.controlStmtContext = r.controlStmtContext;

  -- TODO: handle return void
  local tmpNameStr :: String = "__tmp" ++ toString(genInt());
  local tmpName :: Name = name(tmpNameStr);
  local tmpDecl :: Stmt =
    declStmt(
      variableDecls(nilStorageClass(), nilAttribute(),
        directTypeExpr(e.typerep),
        foldDeclarator([
          declarator(
            tmpName,
            baseTypeExpr(),
            nilAttribute(),
            justInitializer(exprInitializer(e))
          )
        ])
      )
    );

  -- Cilk_set_result(_cilk_ws, &tmpName, sizeof(tmpName));
  local setResult :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_set_result"),
        foldExpr([
          declRefExpr(name("_cilk_ws")),
          mkAddressOf(declRefExpr(tmpName)),
          sizeofExpr(
            exprExpr(declRefExpr(tmpName))
          )
        ])
      )
    );

  local setNoResult :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_SET_NORESULT")),
      -- Cilk_set_result(_cilk_ws, (void *)0, 0);
      exprStmt(
        directCallExpr(
          name("Cilk_set_result"),
          foldExpr([
            declRefExpr(name("_cilk_ws")),
            explicitCastExpr(
              typeName(
                directTypeExpr(builtinType(nilQualifier(), voidType())),
                pointerTypeExpr(nilQualifier(), baseTypeExpr())
              ),
              mkIntConst(0)
            ),
            mkIntConst(0)
          ])
        )
      )
    ]);

  local mSetResult :: Stmt =
    case me of
      justExpr(_)   -> foldStmt([tmpDecl, setResult])
    | nothingExpr() -> setNoResult
    end;

  -- expand CILK2C_BEFORE_RETURN_SLOW() macro
  local beforeSlowReturn :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_BEFORE_RETURN_SLOW macro")),
      -- Cilk_cilk2c_before_return_slow_cp(_cilk_ws, &(_cilk_frame->header));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_before_return_slow_cp"),
          foldExpr([
            declRefExpr(name("_cilk_ws")),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame")),
                true,
                name("header")
              ),
              builtinLoc(MODULE_NAME)
            )
          ])
        )
      ),
      -- Cilk_cilk2c_before_return_slow(_cilk_ws, &(_cilk_frame->header), sizeof(*_cilk_frame));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_before_return_slow"),
          foldExpr([
            declRefExpr(name("_cilk_ws")),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame")),
                true,
                name("header")
              ),
              builtinLoc(MODULE_NAME)
            ),
            sizeofExpr(
              exprExpr(
                dereferenceExpr(
                  declRefExpr(name("_cilk_frame"))
                )
              )
            )
          ])
        )
      )
    ]);

  forwards to
    compoundStmt(
      foldStmt([
        mSetResult,
        beforeSlowReturn,
        returnStmt(nothingExpr())
      ])
    );
}

