grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_returnStmt
r::Stmt ::= e::MaybeExpr
{
  r.pp = concat ([ text("cilk"), space(), text("return"), space(), parens(e.pp) ]);

  -- r.env depends on these, if not set then compiler will crash while looping
  --  in forwarded returnStmt to look for these
  r.globalDecls := e.globalDecls;
  r.defs = e.defs;
  r.freeVariables = e.freeVariables;
  r.functiondefs = [];

  local fast::Boolean = !null(lookupMisc(cilk_in_fast_clone_id, r.env));
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, r.env));

  forwards to case fast,slow of
    | true,false  -> cilk_fastCloneReturn(e)
    | false,true  -> cilk_slowCloneReturn(e)
    | true,true   -> error ("We think we're in both a fast and a slow clone!")
    | false,false -> error ("We don't think we're in a fast or slow clone!")
    end;
}

{- A return in a fast clone stores the result to return in a temporary,
   calls CILK2C_BEFORE_RETURN_FAST();, then returns the temp.
 -}
abstract production cilk_fastCloneReturn
r::Stmt ::= e::MaybeExpr
{
  local tempInt::Integer = genInt();

  -- TODO: check if needs_sync? (see cilk2c/transform.c:TransformReturn())

  -- ToDo: extract return type of the function from env and use below.
  -- Now we assume the return type is int.
  -- or use gcc type-of thing.
  forwards to
    compoundStmt(
      foldStmt(
        case e.justTheExpr of
        | just(e) ->
            [ mkIntDeclGeneral(
                -- TODO: cilk2c numbers tmps (e.g. _cilk_temp0), is this necessary?
                "_cilk_tmp",
                justInitializer(exprInitializer(e)),
                e.location),
              txtStmt("Cilk_cilk2c_before_return_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
              txtStmt("Cilk_cilk2c_before_return_fast(_cilk_ws, &(_cilk_frame->header), sizeof(*_cilk_frame));"),
              txtStmt("return _cilk_tmp;")
            ]
        | nothing() ->
           [ txtStmt("Cilk_cilk2c_before_return_fast_cp(_cilk_ws, &(_cilk_frame->header));"),
             txtStmt("Cilk_cilk2c_before_return_fast(_cilk_ws, &(_cilk_frame->header), sizeof(*_cilk_frame));"),
             txtStmt("return;") ]
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
  local e :: Expr =
    case me of
      justExpr(e1)  -> e1
    | nothingExpr() -> error("internal error in cilk_slowCloneReturn, attempting to extract from nothingExpr()")
    end;
  e.env = r.env;

  -- TODO: handle return void
  local tmpName :: Name = name("__tmp", location=builtIn());
  local tmpDecl :: Stmt =
    declStmt(
      variableDecls([], [],
        directTypeExpr(e.typerep),
        foldDeclarator([
          declarator(
            tmpName,
            baseTypeExpr(),
            [],
            justInitializer(exprInitializer(e))
          )
        ])
      )
    );

  local setResult :: Stmt = txtStmt("Cilk_set_result(_cilk_ws, &__tmp, sizeof(__tmp));");
  local beforeSlowReturn :: Stmt = txtStmt("CILK2C_BEFORE_RETURN_SLOW();");

  forwards to
    compoundStmt(
      foldStmt([
        tmpDecl,
        setResult,
        beforeSlowReturn,
        returnStmt(nothingExpr())
      ])
    );

}

