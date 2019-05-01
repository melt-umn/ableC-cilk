grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

abstract production non_cilk_spawn
top::Stmt ::= l::Maybe<Expr> f::Expr args::Exprs
{
  propagate substituted;
  top.pp = ppConcat([ text("spawn"), space(), 
                    if l.isJust 
                    then ppConcat([l.fromJust.pp, space(), text("="), space()])
                    else ppConcat([]),
                    f.pp, parens( ppImplode(text(","), args.pps) ) ]);
  top.functionDefs := [];

  local lErrors :: [Message] =
    case f of
    | declRefExpr(_) -> []
    | _ -> [err(f.location, "spawns not inside Cilk functions can only be performed by function name (for now)")]
    end;

  local fNm :: String =
    case f of
    | declRefExpr(name(n)) -> n
    end;

  local returnsVoid :: Boolean =
    case f.typerep of
    | functionType(builtinType(_, voidType()), _, _) -> true
    | _ -> false
    end;

  local loc::Location =
    builtinLoc(MODULE_NAME);

  local fwrd :: Stmt =
    compoundStmt(seqStmt(
      performLocked("_cilk_join_lock",
        exprStmt(postIncExpr(useVariable("_cilk_join_counter", loc), location=loc)), loc),
      seqStmt(
        declStmt(variableDecls(nilStorageClass(), nilAttribute(),
          directTypeExpr(pointerType(nilQualifier(), extType(
            nilQualifier(), refIdExtType(structSEU(), 
              s"_cilk_${fNm}_wrapper_struct", s"edu:umn:cs:melt:exts:ableC:cilk:wrapper:${fNm}")))),
          consDeclarator(
            declarator(name("__f", location=loc), 
              baseTypeExpr(), nilAttribute(), nothingInitializer()),
            nilDeclarator()))),
        seqStmt(
          initStruct(fNm, args, l, returnsVoid, loc),
          sendMessage(fNm, loc)))));

  forwards to
    if !null(lErrors)
    then warnStmt(lErrors)
    else fwrd;
}

abstract production non_cilk_sync
top::Stmt ::= loc::Location
{
  top.pp = text("spawn;");

  forwards to
    performLocked(
      "_cilk_join_lock",
      whileStmt(
        notEqualsExpr(
          useVariable("_cilk_join_counter", loc),
          mkIntConst(0, loc),
          location=loc),
        exprStmt(directCallExpr(
          name("pthread_cond_wait", location=loc),
          foldExpr([
            addressOfExpr(useVariable("_cilk_join_cv", loc), location=loc),
            addressOfExpr(useVariable("_cilk_join_lock", loc), location=loc)]),
          location=loc))
      ), loc);
}

function makeWrapperFunction
Decl ::= nm::Name resultType::BaseTypeExpr retMty::TypeModifierExpr
              args::Parameters returnsVoid::Boolean env::Decorated Env
{
  local loc :: Location = builtinLoc(MODULE_NAME);
  local fNm :: String = nm.name;

  args.returnType = nothing();
  args.env = emptyEnv();
  args.position = 0;

  local frameItems :: StructItemList =
    foldStructItem(
      structItem(nilAttribute(),
        typedefTypeExpr(nilQualifier(), name("CilkStackFrame", location=loc)),
        foldStructDeclarator([
          structField(name("header", location=loc), baseTypeExpr(), nilAttribute())
      ])) ::
      structItem(nilAttribute(),
        builtinTypeExpr(nilQualifier(), signedType(intType())),
        foldStructDeclarator([
          structField(name("join_counter", location=loc), 
            pointerTypeExpr(nilQualifier(), baseTypeExpr()), nilAttribute())
      ])) ::
      structItem(nilAttribute(),
        typedefTypeExpr(nilQualifier(), name("pthread_mutex_t", location=loc)),
        foldStructDeclarator([
          structField(name("join_lock", location=loc),
            pointerTypeExpr(nilQualifier(), baseTypeExpr()), nilAttribute())
      ])) ::
      structItem(nilAttribute(),
        typedefTypeExpr(nilQualifier(), name("pthread_cond_t", location=loc)),
        foldStructDeclarator([
          structField(name("join_cv", location=loc),
            pointerTypeExpr(nilQualifier(), baseTypeExpr()), nilAttribute())
      ])) ::
     (if returnsVoid
      then []
      else structItem(nilAttribute(),
        typeModifierTypeExpr(resultType, retMty),
        foldStructDeclarator(
          structField(name("ret", location=loc),
            pointerTypeExpr(nilQualifier(), baseTypeExpr()), nilAttribute())
          :: structField(name("res", location=loc), baseTypeExpr(), nilAttribute())
          :: [])) :: [])
      ++
      createStructEntries(args, 0, loc)
    );

  local funcBody :: Stmt =
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_start_thread_slow_cp", loc)),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_start_thread_slow", loc)),
    seqStmt(switchStmt(memberExpr(
      memberExpr(useVariable("_cilk_frame", loc), true, 
        name("header", location=loc), location=loc),
      false, name("entry", location=loc), location=loc),
      seqStmt(
        caseLabelStmt(mkIntConst(1, loc), gotoStmt(name("_cilk_sync1", location=loc))),
      seqStmt(caseLabelStmt(mkIntConst(2, loc), gotoStmt(name("_cilk_sync2", location=loc))),
      caseLabelStmt(mkIntConst(3, loc), gotoStmt(name("_cilk_sync3", location=loc)))))),
    seqStmt(exprStmt(eqExpr(
      memberExpr(
        memberExpr(useVariable("_cilk_frame", loc), true, 
          name("header", location=loc), location=loc),
        false, name("entry", location=loc), location=loc),
      mkIntConst(1, loc), location=loc)), 
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_before_spawn_slow_cp", loc)),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_push_frame", loc)),
    seqStmt(
      let call :: Expr =
        directCallExpr(name(fNm, location=loc), 
          foldExpr(useVariable("_cilk_ws", loc) ::
          callArgs(args, 0, loc)), location=loc)
      in
      if returnsVoid
      then exprStmt(call)
      else seqStmt(mkDecl("res", 
        (decorate typeModifierTypeExpr(resultType, retMty) with 
          {returnType=nothing(); env=env; givenRefId=nothing(); }).typerep, 
        call, loc),
        exprStmt(eqExpr(memberExpr(
          useVariable("_cilk_frame", loc), true, name("res", location=loc),
          location=loc), useVariable("res", loc), location=loc)))
      end,
    seqStmt(ifStmtNoElse(callWithWS("Cilk_cilk2c_pop_check", loc),
      ifStmtNoElse(directCallExpr(
        name("Cilk_exception_handler", location=loc),
        foldExpr(useVariable("_cilk_ws", loc) :: 
          if returnsVoid
          then explicitCastExpr(
                  typeName(builtinTypeExpr(nilQualifier(), voidType()), 
                    pointerTypeExpr(nilQualifier(), baseTypeExpr())),
                  mkIntConst(0, loc), location=loc) 
               :: mkIntConst(0, loc) :: []
          else addressOfExpr(useVariable("res", loc), location=loc) :: 
               sizeofExpr(typeNameExpr(typeName(resultType, retMty)), 
                location=loc) 
               :: []),
        location=loc),
        seqStmt(exprStmt(callWithWS("Cilk_cilk2c_pop", loc)),
          returnStmt(nothingExpr())))),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_after_spawn_slow_cp", loc)),
    seqStmt(labelStmt(name("_cilk_sync1", location=loc), nullStmt()),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_at_thread_boundary_slow_cp", loc)),
    seqStmt(exprStmt(callWithWS("Cilk_cilk2c_event_new_thread_maybe", loc)),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_before_sync_slow_cp", loc)),
    seqStmt(exprStmt(eqExpr(
      memberExpr(memberExpr(useVariable("_cilk_frame", loc), true, 
        name("header", location=loc), location=loc), false,
        name("entry", location=loc), location=loc),
      mkIntConst(2, loc), location=loc)),
    seqStmt(ifStmtNoElse(callWithWS("Cilk_sync", loc), seqStmt(
      returnStmt(nothingExpr()), labelStmt(name("_cilk_sync2", location=loc), nullStmt()))),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_after_sync_slow_cp", loc)),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_at_thread_boundary_slow_cp", loc)),
    seqStmt(exprStmt(callWithWS("Cilk_cilk2c_event_new_thread_maybe", loc)),
    seqStmt(
      if returnsVoid
      then nullStmt()
      else ifStmtNoElse(memberExpr(useVariable("_cilk_frame", loc), true,
        name("ret", location=loc), location=loc), 
        exprStmt(eqExpr(dereferenceExpr(memberExpr(
            useVariable("_cilk_frame", loc), true, 
            name("ret", location=loc), location=loc), location=loc), 
          memberExpr(useVariable("_cilk_frame", loc), true,
            name("res", location=loc), location=loc), location=loc))),
    seqStmt(exprStmt(directCallExpr(name("pthread_mutex_lock", location=loc),
      foldExpr([memberExpr(useVariable("_cilk_frame", loc),
        true, name("join_lock", location=loc), location=loc)]), 
      location=loc)),
    seqStmt(exprStmt(subEqExpr(
      dereferenceExpr(memberExpr(useVariable("_cilk_frame", loc),
        true, name("join_counter", location=loc), location=loc), location=loc), 
      mkIntConst(1, loc), location=loc)),
    seqStmt(exprStmt(directCallExpr(name("pthread_cond_signal", location=loc),
      foldExpr([memberExpr(useVariable("_cilk_frame", loc),
        true, name("join_cv", location=loc), location=loc)]), 
      location=loc)),
    seqStmt(exprStmt(directCallExpr(name("pthread_mutex_unlock", location=loc),
      foldExpr([memberExpr(useVariable("_cilk_frame", loc),
        true, name("join_lock", location=loc), location=loc)]), 
      location=loc)),
    seqStmt(exprStmt(directCallExpr(name("Cilk_remove_and_free_closure_and_frame", location=loc),
      foldExpr(useVariable("_cilk_ws", loc) ::
        addressOfExpr(memberExpr(useVariable("_cilk_frame", loc), true,
          name("header", location=loc), location=loc), location=loc) ::
        memberExpr(useVariable("_cilk_ws", loc), true, name("self", location=loc),
          location=loc) :: []),
        location=loc)),
    seqStmt(returnStmt(nothingExpr()),
    seqStmt(labelStmt(name("_cilk_sync3", location=loc), nullStmt()),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_after_sync_slow_cp", loc)),
    seqStmt(exprStmt(callWithWSHeader("Cilk_cilk2c_at_thread_boundary_slow_cp", loc)),
    exprStmt(callWithWS("Cilk_cilk2c_event_new_thread_maybe", loc))))))))))))))))))))))))))))));

  local structDec :: Decl = typeExprDecl(nilAttribute(),
    structTypeExpr(nilQualifier(), structDecl(
      nilAttribute(), justName(name(s"_cilk_${fNm}_wrapper_struct", location=loc)),
      frameItems, location=loc)));

  local functionDec :: Decl = functionDeclaration(
    functionDecl(foldStorageClass([staticStorageClass()]), nilSpecialSpecifier(),
    directTypeExpr(builtinType(nilQualifier(), voidType())), 
    functionTypeExprWithArgs(baseTypeExpr(), 
      foldParameterDecl(
				parameterDecl(
					nilStorageClass(),
					typedefTypeExpr(nilQualifier(), name("CilkWorkerState", location=loc)),
					pointerTypeExpr(foldQualifier([constQualifier(location=loc)]), baseTypeExpr()),
					justName(name("_cilk_ws", location=loc)),
					nilAttribute()
				) ::
				parameterDecl(
					nilStorageClass(),
          directTypeExpr(extType(nilQualifier(), refIdExtType(structSEU(), 
              s"_cilk_${fNm}_wrapper_struct", s"edu:umn:cs:melt:exts:ableC:cilk:wrapper:${fNm}"))),
					pointerTypeExpr(nilQualifier(), baseTypeExpr()),
					justName(name("_cilk_frame", location=loc)),
					nilAttribute()
				) :: []), 
      false, nilQualifier()), 
    name(s"_cilk_${fNm}_wrapper", location=loc), 
    nilAttribute(), nilDecl(), funcBody));

  local sigDec :: Decl = variableDecls(foldStorageClass([staticStorageClass()]),
    nilAttribute(), typedefTypeExpr(nilQualifier(), name("CilkProcInfo", location=loc)),
    consDeclarator(
      declarator(name(s"_cilk_${fNm}_wrapper_sig", location=loc),
      arrayTypeExprWithoutExpr(baseTypeExpr(), nilQualifier(), normalArraySize()),
      nilAttribute(),
      justInitializer(objectInitializer(
        consInit(positionalInit(objectInitializer(
          consInit(positionalInit(exprInitializer(
            if returnsVoid
            then mkIntConst(0, loc)
            else sizeofExpr(typeNameExpr(typeName(resultType, retMty)), location=loc)
          )), 
          consInit(positionalInit(exprInitializer(
            sizeofExpr(typeNameExpr(typeName(directTypeExpr(extType(nilQualifier(),
              refIdExtType(structSEU(), s"_cilk_${fNm}_wrapper_struct",
              s"edu:umn:cs:melt:exts:ableC:cilk:wrapper:${fNm}"))), baseTypeExpr())), location=loc)
          )),
          consInit(positionalInit(exprInitializer(useVariable(s"_cilk_${fNm}_wrapper", loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), nilInit())))))
        )),
        consInit(positionalInit(objectInitializer(
          consInit(positionalInit(exprInitializer(
            if returnsVoid
            then mkIntConst(0, loc)
            else sizeofExpr(typeNameExpr(typeName(resultType, retMty)), location=loc)
          )), 
          consInit(positionalInit(exprInitializer(
            if returnsVoid
            then mkIntConst(0, loc)
            else explicitCastExpr(typeName(typedefTypeExpr(nilQualifier(),
              name("size_t", location=loc)), baseTypeExpr()), 
              addressOfExpr(memberExpr(
                explicitCastExpr(typeName(directTypeExpr(extType(nilQualifier(),
                  refIdExtType(structSEU(), s"_cilk_${fNm}_wrapper_struct",
                    s"edu:umn:cs:melt:exts:ableC:cilk:wrapper:${fNm}"))),
                  pointerTypeExpr(nilQualifier(), baseTypeExpr())),
                  mkIntConst(0, loc), location=loc),
                true,
                name("res", location=loc), location=loc), location=loc), location=loc)
          )),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), nilInit())))))
        )),
        consInit(positionalInit(objectInitializer(
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), nilInit())))))
        )),
        consInit(positionalInit(objectInitializer(
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), 
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))),
          consInit(positionalInit(exprInitializer(mkIntConst(0, loc))), nilInit())))))
        )),
        nilInit()))))
      ))), 
    nilDeclarator()));

  return decls(consDecl(structDec, consDecl(functionDec, consDecl(sigDec, nilDecl()))));
}

function createStructEntries
[StructItem] ::= param::Decorated Parameters count::Integer loc::Location
{
  return
    case param of
    | consParameters(h, t) ->
      structItem(nilAttribute(),
        directTypeExpr(h.typerep),
        foldStructDeclarator([
          structField(name("arg" ++ toString(count), location=loc), baseTypeExpr(), nilAttribute())
        ])
      ) :: createStructEntries(t, count+1, loc)
    | nilParameters() -> []
    end;
}

function callArgs
[Expr] ::= param::Decorated Parameters count::Integer loc::Location
{
  return 
    case param of
    | consParameters(h, t) ->
      memberExpr(useVariable("_cilk_frame", loc), true, 
        name(s"arg${toString(count)}", location=loc), location=loc) 
      :: callArgs(t, count+1, loc)
    | _ -> []
    end;
}

function initStruct
Stmt ::= fNm::String args::Exprs l::Maybe<Expr> returnsVoid::Boolean loc::Location
{
  local initArgs :: Stmt =
    initializeArgs(args, 0, loc);

  return
    seqStmt(
      exprStmt(eqExpr(useVariable("__f", loc),
        directCallExpr(name("Cilk_malloc", location=loc),
          foldExpr([sizeofExpr(typeNameExpr(typeName(directTypeExpr(extType(
            nilQualifier(), refIdExtType(structSEU(), 
              s"_cilk_${fNm}_wrapper_struct", s"edu:umn:cs:melt:exts:ableC:cilk:wrapper:${fNm}"))), 
            baseTypeExpr())), location=loc)]),
          location=loc), location=loc)),
      seqStmt(
      exprStmt(eqExpr(memberExpr(useVariable("__f", loc), true, name("join_counter", location=loc), location=loc),
        addressOfExpr(useVariable("_cilk_join_counter", loc), location=loc), location=loc)),
      seqStmt(
        exprStmt(eqExpr(memberExpr(useVariable("__f", loc), true, name("join_lock", location=loc), location=loc),
          addressOfExpr(useVariable("_cilk_join_lock", loc), location=loc), location=loc)),
      seqStmt(
        exprStmt(eqExpr(memberExpr(useVariable("__f", loc), true, name("join_cv", location=loc), location=loc),
          addressOfExpr(useVariable("_cilk_join_cv", loc), location=loc), location=loc)),
      seqStmt(
        if returnsVoid
        then nullStmt()
        else exprStmt(eqExpr(memberExpr(useVariable("__f", loc), true, 
          name("ret", location=loc), location=loc),
          if l.isJust
          then addressOfExpr(l.fromJust, location=loc)
          else explicitCastExpr(
            typeName(builtinTypeExpr(nilQualifier(), voidType()), pointerTypeExpr(nilQualifier(), baseTypeExpr())), 
            mkIntConst(0, loc), location=loc),
          location=loc)),
      initArgs)))));
}

function sendMessage
Stmt ::= fNm::String loc::Location
{
  return seqStmt(
    declStmt(variableDecls(nilStorageClass(), nilAttribute(),
      typedefTypeExpr(nilQualifier(), name("thread_pool", location=loc)),
      consDeclarator(
        declarator(name("__pool", location=loc), 
          baseTypeExpr(), nilAttribute(), nothingInitializer()),
        nilDeclarator()))),
    seqStmt(declStmt(variableDecls(nilStorageClass(), nilAttribute(),
      typedefTypeExpr(nilQualifier(), name("thread_message", location=loc)),
      consDeclarator(
        declarator(name("__rsp", location=loc), 
          baseTypeExpr(), nilAttribute(), nothingInitializer()),
        nilDeclarator()))),
    seqStmt(declStmt(variableDecls(nilStorageClass(), nilAttribute(),
      directTypeExpr(extType(
        nilQualifier(), refIdExtType(structSEU(), 
          s"cilk_message", s"edu:umn:cs:melt:exts:ableC:cilk:message"))),
      consDeclarator(
        declarator(name("__msg", location=loc), 
          baseTypeExpr(), nilAttribute(), nothingInitializer()),
        nilDeclarator()))),
    seqStmt(
      exprStmt(eqExpr(memberExpr(useVariable("__msg", loc), false, name("args", location=loc), location=loc),
        useVariable("__f", loc), location=loc)),
    seqStmt(
      exprStmt(eqExpr(memberExpr(useVariable("__msg", loc), false, name("sig", location=loc), location=loc),
        useVariable(s"_cilk_${fNm}_wrapper_sig", loc), location=loc)),
    seqStmt(
      exprStmt(directCallExpr(
        name("get_thread_pool", location=loc),
        foldExpr(mkIntConst(2, loc) :: -- TODO: Should use Cilk_num rather than 2
          addressOfExpr(useVariable("__pool", loc), location=loc) :: []),
        location=loc
      )),
    seqStmt(
      exprStmt(directCallExpr(
        name("send_message", location=loc),
        foldExpr(useVariable("__pool", loc) :: 
          addressOfExpr(useVariable("__msg", loc), location=loc) ::
          addressOfExpr(useVariable("__rsp", loc), location=loc) :: []),
        location=loc
      )),
    seqStmt(
      exprStmt(directCallExpr(
        name("receive_response", location=loc),
        foldExpr([useVariable("__rsp", loc)]), location=loc)),
    nullStmt()))))))));
}

function initializeArgs
Stmt ::= args::Exprs cnt::Integer loc::Location
{
  return
    case args of
    | nilExpr() -> nullStmt()
    | consExpr(h, tl) -> seqStmt(
      exprStmt(eqExpr(memberExpr(useVariable("__f", loc), true, 
                        name(s"arg${toString(cnt)}", location=loc), location=loc), h, location=loc)),
      initializeArgs(tl, cnt+1, loc))
    end;
}

function useVariable
Expr ::= varName::String loc::Location
{
  return
    declRefExpr(name(varName, location=loc), location=loc);
}

function callWithWSHeader
Expr ::= fNm::String loc::Location
{
  return directCallExpr(
      name(fNm, location=loc),
      foldExpr(useVariable("_cilk_ws", loc) ::
        addressOfExpr(memberExpr(useVariable("_cilk_frame", loc), true, 
          name("header", location=loc), location=loc), location=loc) :: []), 
      location=loc);
}

function callWithWS
Expr ::= fNm::String loc::Location
{
  return directCallExpr(
      name(fNm, location=loc),
      foldExpr(useVariable("_cilk_ws", loc) :: []), location=loc);
}

function performLocked
Stmt ::= lockName::String body::Stmt loc::Location
{
  return
    seqStmt(
      exprStmt(directCallExpr(
        name("pthread_mutex_lock", location=loc),
        foldExpr([addressOfExpr(useVariable(lockName, loc), location=loc)]),
        location=loc)),
      seqStmt(
        body,
        exprStmt(directCallExpr(
          name("pthread_mutex_unlock", location=loc),
          foldExpr([addressOfExpr(useVariable(lockName, loc), location=loc)]),
          location=loc))
      )
    );
}
