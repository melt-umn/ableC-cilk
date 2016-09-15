grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_returnStmt
r::Stmt ::= e::MaybeExpr
{
  r.pp = concat ([ text("cilk"), space(), text("return"), space(), parens(e.pp) ]);
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

  -- ToDo: extract return type of the function from env and use below.
  -- Now we assume the return type is int.  
  -- or use gcc type-of thing.
  forwards to 
    compoundStmt(
      foldStmt ( 
        case e.justTheExpr of
        | just(e) ->
            [ mkIntDeclGeneral(
                "_cilk_tmp",
                justInitializer(exprInitializer(e)),
                e.location),
              txtStmt("CILK2C_BEFORE_RETURN_FAST();"), 
              txtStmt("return _cilk_tmp;")
            ]
        | nothing() -> 
           [ txtStmt("CILK2C_BEFORE_RETURN_FAST();"), 
             txtStmt("return _cilk_tmp;") ]
        end ) ) ;
}



{- A return in a slow clone stores the result to return in a
   temporary, calls Cilk_set_result to store this in the frame,
   calls CILK2C_BEFORE_RETURN_SLOW(), then rturns nothing.  Recall
   that the return type of the slow clone is void.
 -}
abstract production cilk_slowCloneReturn
r::Stmt ::= e::MaybeExpr
{

  forwards to nullStmt();
{-
    compoundStmt(
      foldStmt( [
        compoundStmt(
        )
        txtStmt("");
-}

}
