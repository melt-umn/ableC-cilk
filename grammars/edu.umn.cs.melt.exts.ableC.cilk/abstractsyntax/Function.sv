grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:overloadable as ovrld;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

aspect production functionDeclaration
top::Decl ::= f::FunctionDecl
{
--  top.globalDecls <- [pair("CILK_WHERE_AM_I", inCCode())];
}

{- somewhat similar to cilkc2c/transform.c:TransformCilkProc() -}
abstract production cilkFunctionDecl
top::Decl ::= storage::StorageClasses  fnquals::SpecialSpecifiers
  bty::BaseTypeExpr mty::TypeModifierExpr  fname::Name  attrs::Attributes
  dcls::Decls  body::Stmt
{
  -- ToDo: check that storage, fnquals, and attrs are empty
      -- or just remove them?  I guess supporting them in concrete syntax
      -- could lead to nicer error messages than a parse error.

  bty.givenRefId = nothing();

  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;

  top.pp = ppConcat([
      terminate(space(), storage.pps),
      terminate( space(), fnquals.pps ),
      bty.pp, space(), mty.lpp, fname.pp, mty.rpp,
      ppAttributesRHS(attrs), line(),
      terminate(cat(semi(), line()), dcls.pps),
      text("{"), line(), nestlines(2,body.pp), text("}")
    ]);

  fnquals.env = top.env;
  fnquals.controlStmtContext = top.controlStmtContext;

  local newName :: Name = case fname.name of
                          | "main" -> name("cilk_main", location=fname.location)
                          | _ -> fname
                          end;

{- ToDo: attributes for recovering the following information that must
   precede a function in the generated C code.
   1. Frame struct declaration
   2. Argument struct declaration
   3. Prototype of slow clone
   4. ProcInfo definition
   ... more
 -}

-- frame struct --------------------------------------------------
-- should be able to collect this in a syn attr, perhaps even just
-- pulling things out defs in the places that they are added to the env.
  local frameStruct :: Decl = makeFrame(newName, args, body);

-- arg struct --------------------------------------------------
-- again, another syn attr or scope0 of the frame struct information
  local argStruct :: Decl = makeArgsAndResultStruct(newName, bty, retMty, args);

  local args :: Parameters =
    case mty of
    | functionTypeExprWithArgs(_, args1, _, _) -> args1
    | functionTypeExprWithoutArgs(_, _, _)     -> nilParameters()
    | _ -> error("ToDo: fix this in Cilk ext.  Violating some rules about extensibility.")
    end;

-- Slow Clone --------------------------------------------------

  local returnsVoid :: Boolean =
    case bty of
      builtinTypeExpr(_, voidType()) -> true
    | _                              -> false
    end;

  local retMty :: TypeModifierExpr =
    case mty of
    | functionTypeExprWithArgs(ret, _, _, _) -> ret
    | functionTypeExprWithoutArgs(ret, _, _) -> ret
    | _ -> error("expected mty to be functionTypeExpr")
    end;

  -- TODO: only add implicit sync/return if necessary
  -- `sync; cilk_return;' is always necessary so add it just in case user didn't include it
  local newBody :: Stmt =
    if   returnsVoid
    then foldStmt([
           body, cilk_syncStmt(fname.location), cilk_returnStmt(nothingExpr())
         ])
    else foldStmt([body, cilk_syncStmt(fname.location)]);

  local slowCloneDecl :: Decl = slowClone(newName, dcls, slowCloneBody);
  local slowCloneBody :: Stmt = transformSlowClone(newBody, args);

  slowCloneBody.env = top.env;
  slowCloneBody.cilkLinksInh = [];
  slowCloneBody.controlStmtContext = initialControlStmtContext;
  slowCloneBody.cilkProcName = newName;

---- Proc Info --------------------------------------------------
  local linkage :: Decl = makeLinkage(newName, bty, slowCloneBody.cilkLinks, returnsVoid);

-- Fast Clone --------------------------------------------------
  local fastCloneDecl :: Decl = fastClone(bty, mty, newName, dcls, fastCloneBody);
  local fastCloneBody :: Stmt = transformFastClone(newBody, newName, args);

-- Import Function --------------------------------------------------
  local importBody :: Stmt = makeImportBody(newName, args, returnsVoid);
  local importDecl :: Decl = makeImportFunction(newName, importBody);

-- Export Function --------------------------------------------------
  local exportBody :: Stmt = makeExportBody(newName, bty, retMty, args, returnsVoid);
  local exportDecl :: Decl = makeExportFunction(newName, bty, retMty, args, exportBody);

  forwards to 
    decls(foldDecl([
      frameStruct,
      argStruct,
      slowCloneDecl,
      linkage,
      fastCloneDecl,
      importDecl,
      exportDecl
    ]));
}

abstract production cilkFunctionProto
top::Decl ::= storage::StorageClasses  fnquals::SpecialSpecifiers
  bty::BaseTypeExpr mty::TypeModifierExpr  fname::Name  attrs::Attributes
{
  top.pp = ppConcat([text("cilk "), terminate(space(), storage.pps), terminate( space(), fnquals.pps ),
    bty.pp, space(), mty.lpp, fname.pp, mty.rpp, ppAttributesRHS(attrs), line(),
    semi()]);

  bty.givenRefId = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;

  local slowName :: Name = name("_cilk_" ++ fname.name ++ "_slow", location=builtinLoc(MODULE_NAME));
  local void :: BaseTypeExpr = directTypeExpr(builtinType(nilQualifier(), voidType()));

  local slowMty :: TypeModifierExpr =
    functionTypeExprWithArgs(baseTypeExpr(), mkSlowParams(fname), false, nilQualifier());

  -- TODO: do all of these need to be forward declared?
  local frameDecl :: Decl = txtDecl("struct _cilk_" ++ fname.name ++ "_frame;");
  local argsDecl :: Decl = txtDecl("struct _cilk_" ++ fname.name ++ "_args;");
  local importProto :: Decl = txtDecl("static void _cilk_" ++ fname.name ++
    "_import(CilkWorkerState *const _cilk_ws, void *_cilk_procargs_v);");
  -- TODO: does export need to be forward declared?
--  local exportProto :: Decl = txtDecl("int mt_" ++ fname.name ++
--    "CilkContext *const context, ...args););

  local slowProto :: Decl =
    variableDecls(
      foldStorageClass([staticStorageClass()]),
      nilAttribute(),
      void,
      consDeclarator(
        declarator(
          slowName, slowMty, attrs, nothingInitializer()
        ),
        nilDeclarator()
      )
    );

  local fastProto :: Decl =
    variableDecls(
      storage, attrs, bty,
      consDeclarator(
        declarator(
          fname, addWsToParams(mty), attrs, nothingInitializer()
        ),
        nilDeclarator()
      )
    );

  forwards to
    decls(foldDecl([
    -- TODO: why do we get the following error when declaring slowProto?
    -- Built In:0:0: error: Redeclaration of _cilk_fib_slow with incompatible types.
    -- Original (from line 0)  void(CilkWorkerState const*, struct _cilk_fib_frame *)
    -- but here it is          void(CilkWorkerState const*, struct _cilk_fib_frame *)

      frameDecl,
      argsDecl,
--      slowProto,
      fastProto,
      importProto
    ]));
}

{- Note that both fastClone and slowClone include all the (allowed)
   children from the original AST in the clones that are forwarded to.

   This is potentially critical for some versions of non-interference.
 -}

global cilk_in_fast_clone_id::String = "cilk_in_fast_clone";
global cilk_in_slow_clone_id::String = "cilk_in_slow_clone";
global cilk_sync_locations_id::String = "cilk_sync_locations_id";

{- based on cilkc2c/transform.c:MakeFrame()

   struct _cilk_foo_frame {
     CilkStackFrame header;
     { ... args ... } scope0;
     { ... scope 1 vars ... } scope1;
     { ... scope <n> vars ... } scope<n>;
   };
-}
abstract production makeFrame
top::Decl ::= newName::Name args::Parameters body::Stmt
{
  top.pp = text("cilkMakeFrame()");
  local header :: StructItem =
    structItem(
      nilAttribute(),
      typedefTypeExpr(nilQualifier(), name("CilkStackFrame", location=builtinLoc(MODULE_NAME))),
      foldStructDeclarator([
        structField(name("header", location=builtinLoc(MODULE_NAME)), baseTypeExpr(), nilAttribute())
      ])
    );

  -- collect all fields with the same scopeId into a list and pair with the scopeId
  local frameDeclsByScopes :: [Pair<String [StructItem]>] =
    collectFrameDecls(args.cilkFrameDeclsScopes ++ body.cilkFrameDeclsScopes, []);

  local frameFields :: [StructItem] =
    cons(header, map(makeFrameDeclsScope, frameDeclsByScopes));
  
  args.position = 0;
  body.env = top.env;

  forwards to
    typeExprDecl(nilAttribute(),
      structTypeExpr(
        nilQualifier(),
        structDecl(
          nilAttribute(),
          justName(name("_cilk_" ++ newName.name ++ "_frame", location=builtinLoc(MODULE_NAME))),
          foldStructItem(frameFields),
          location=builtinLoc(MODULE_NAME)
        )
      )
    );
}

function collectFrameDecls
[Pair<String [StructItem]>] ::= cilkFrameDecls::[Pair<String StructItem>]
                             cilkFrameDeclsByScopes::[Pair<String [StructItem]>]
{
  return
    if   null(cilkFrameDecls)
    then cilkFrameDeclsByScopes
    else collectFrameDecls(
           tail(cilkFrameDecls), addDeclToScopes(head(cilkFrameDecls), cilkFrameDeclsByScopes)
         );
}

function addDeclToScopes
[Pair<String [StructItem]>] ::= cilkFrameDecl::Pair<String StructItem>
                             cilkFrameDeclsByScopes::[Pair<String [StructItem]>]
{
  return
    if   null(cilkFrameDeclsByScopes)
    then [pair(fst(cilkFrameDecl), [snd(cilkFrameDecl)])]
    else
      if   fst(head(cilkFrameDeclsByScopes)) == fst(cilkFrameDecl)
      then
        cons(
          pair(
            fst(cilkFrameDecl),
            cons(snd(cilkFrameDecl), snd(head(cilkFrameDeclsByScopes)))
          ),
          tail(cilkFrameDeclsByScopes)
        )
      else
        cons(
          head(cilkFrameDeclsByScopes),
          addDeclToScopes(cilkFrameDecl, tail(cilkFrameDeclsByScopes))
        );
}

-- TODO: choose better function names
function makeFrameDeclsScope
StructItem ::= cilkFrameDecl::Pair<String [StructItem]>
{
  return
    structItem(
      nilAttribute(),
      structTypeExpr(
        nilQualifier(),
        structDecl(
          nilAttribute(),
          justName(name("_cilk_frame_" ++ toString(genInt()), location=builtinLoc(MODULE_NAME))),
          foldStructItem(snd(cilkFrameDecl)),
          location=builtinLoc(MODULE_NAME)
        )
      ),
      foldStructDeclarator([
        structField(
          name("scope" ++ fst(cilkFrameDecl), location=builtinLoc(MODULE_NAME)),
          baseTypeExpr(),
          nilAttribute()
        )
      ])
    );
}

function makeFrameDecls
[StructItem] ::= names::[String]  env::Decorated Env
{
  return
    if   null(names)
    then []
    else cons(makeFrameDecl(head(names), env), makeFrameDecls(tail(names), env));
}

function makeFrameDecl
StructItem ::= n::String  env::Decorated Env
{
  -- TODO: check if lookupValue() returns Nil
  local value :: ValueItem = head(lookupValue(n, env));
  local typerep :: Type = value.typerep;

  return
    structItem(
      nilAttribute(), -- TODO: attributes?
      value.typerep.baseTypeExpr,
      foldStructDeclarator([
        structField(name(n, location=builtinLoc(MODULE_NAME)), value.typerep.typeModifierExpr, nilAttribute())
      ])
    );
}

function wrapFrameDeclsScopes
StructItemList ::= cilkFrameDeclsScopes::[[StructItem]] scopeCount::Integer
{
  return
    if   null(cilkFrameDeclsScopes)
    then nilStructItem()
    else if   null(head(cilkFrameDeclsScopes))
         then wrapFrameDeclsScopes(tail(cilkFrameDeclsScopes), scopeCount)
         else
           consStructItem(
              structItem(
                nilAttribute(),
                structTypeExpr(
                  nilQualifier(),
                  structDecl(
                    nilAttribute(),
                    justName(name("_cilk_frame_" ++ toString(genInt()), location=builtinLoc(MODULE_NAME))),
--                    nilStructItem(),
                    foldStructItem(head(cilkFrameDeclsScopes)),
                    location=builtinLoc(MODULE_NAME)
                  )
                ),
                foldStructDeclarator([
                  structField(
                    name("scope" ++ toString(scopeCount), location=builtinLoc(MODULE_NAME)),
                    baseTypeExpr(),
                    nilAttribute()
                  )
                ])
              ),
              wrapFrameDeclsScopes(tail(cilkFrameDeclsScopes), scopeCount + 1)
            );
}

{- based on cilkc2c/transform.c:MakeArgsAndResultStruct()

   struct _cilk_foo_args {
     return_type _cilk_proc_result; // if foo return type is not void
     ... args ...
   };
-}
abstract production makeArgsAndResultStruct
top::Decl ::= fname::Name  bty::BaseTypeExpr  retMty::TypeModifierExpr  args::Parameters
{
  top.pp = text("cilkMakeArgsAndResultStruct()");

  local structName :: Name = name("_cilk_" ++ fname.name ++ "_args", location=builtinLoc(MODULE_NAME));
  local resultField :: StructItem =
    structItem(
      nilAttribute(),
      bty,
      foldStructDeclarator([
        structField(name("_cilk_proc_result", location=builtinLoc(MODULE_NAME)), retMty, nilAttribute())
      ])
    );

  local argFields :: StructItemList = makeArgFields(args);

  bty.givenRefId = nothing();
  args.position = 0;

  local fields :: StructItemList =
    case bty.typerep of
    | builtinType(_, voidType()) -> argFields
    | _                          -> consStructItem(resultField, argFields)
    end;

  forwards to
    typeExprDecl(nilAttribute(),
      structTypeExpr(
        nilQualifier(),
        structDecl(nilAttribute(), justName(structName), fields, location=builtinLoc(MODULE_NAME))
      )
    );
}

function makeArgFields
StructItemList ::= args::Decorated Parameters
{
  return
    case args of
    | consParameters(h, t) -> consStructItem(makeArgField(h), makeArgFields(t))
    | nilParameters()      -> nilStructItem()
    end;
}

{- FIXME: char *argv[] not supported, must be char **argv -}
function makeArgField
StructItem ::= arg::Decorated ParameterDecl
{
  local n :: Name =
    case arg.paramname of
    | just(n1) -> n1
    | _        -> error("cilk function parameter must be named")
    end;

  local attrs :: Attributes =
    case arg of parameterDecl(_, _, _, _, attrs1) -> attrs1 end;

  local bty :: BaseTypeExpr =
    case arg of parameterDecl(_, bty1, _, _, _) -> bty1 end;

  local mty :: TypeModifierExpr =
    case arg of parameterDecl(_, _, mty1, _, _) -> mty1 end;

  return
    structItem(
      attrs,
      bty,
      foldStructDeclarator([
        structField(n, mty, nilAttribute())
      ])
    );
}

{- TODO: is there any way to refactor this by converting Parameters to
    [ParameterDecl] then mapping a simpler function onto it? -}
function makeArgDecls
Stmt ::= args::Decorated Parameters
{
  return
    case args of
    | consParameters(h, t) -> seqStmt(makeArgDecl(h), makeArgDecls(t))
    | nilParameters()      -> nullStmt()
    end;
}

function makeArgDecl
Stmt ::= arg::Decorated ParameterDecl
{
  local n :: Name =
    case arg.paramname of
    | just(n1) -> n1
    | _        -> error("cilk function parameter must be named")
    end;

  local bty :: BaseTypeExpr =
    case arg of parameterDecl(_, bty1, _, _, _) -> bty1 end;

  local mty :: TypeModifierExpr =
    case arg of parameterDecl(_, _, mty1, _, _) -> mty1 end;

  local attrs :: Attributes =
    case arg of parameterDecl(_, _, _, _, attrs1) -> attrs1 end;

  return
    declStmt(
      variableDecls(
        nilStorageClass(), attrs, bty,
        foldDeclarator([ declarator(n, mty, nilAttribute(), nothingInitializer()) ])
      )
    );
}

function restoreVariables
Stmt ::= env::Decorated Env
{
  -- get all name/scopeIds pairs except those at global scope
  local cilkFrameVars :: [Pair<String String>] =
    foldr(append, [], map(tm:toList, take(length(env.scopeIds)-1, env.scopeIds)));

  return
    foldStmt([
      exprStmt(comment("TODO: restore only live variables", location=builtinLoc(MODULE_NAME))),
      foldStmt(map(restoreVariable, cilkFrameVars))
    ]);
}

function restoreVariable
Stmt ::= cilkFrameVar::Pair<String String>
{
  local n :: String = fst(cilkFrameVar);
  local scopeId :: String = snd(cilkFrameVar);
  local scopeName :: Name = name("scope" ++ scopeId, location=builtinLoc(MODULE_NAME));

  return
    -- TODO: avoid putting _cilk_ws/frame into defs instead of handling as special case
    if   n != "_cilk_ws" && n != "_cilk_frame"
    then
      -- n = _cilk_frame->scopeX.n;
      exprStmt(
        eqExpr(
          declRefExpr(name(n, location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
          memberExpr(
            memberExpr(
              declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
              true,
              scopeName,
              location=builtinLoc(MODULE_NAME)
            ),
            false,
            name(n, location=builtinLoc(MODULE_NAME)),
            location=builtinLoc(MODULE_NAME)
          ),
          location=builtinLoc(MODULE_NAME)
        )
      )
    else
      nullStmt();
}

function saveVariables
Stmt ::= env::Decorated Env
{
  -- get all name/scopeIds pairs except those at global scope
  local cilkFrameVars :: [Pair<String String>] =
    foldr(append, [], map(tm:toList, take(length(env.scopeIds)-1, env.scopeIds)));

  return
    foldStmt([
      exprStmt(comment("TODO: save only live, dirty variables", location=builtinLoc(MODULE_NAME))),
      foldStmt(map(saveVariable, cilkFrameVars))
    ]);
}

function saveVariable
Stmt ::= cilkFrameVar::Pair<String String>
{
  local n :: String = fst(cilkFrameVar);
  local scopeId :: String = snd(cilkFrameVar);
  local scopeName :: Name = name("scope" ++ scopeId, location=builtinLoc(MODULE_NAME));

  return
    -- TODO: avoid putting _cilk_ws/frame into defs instead of handling as special case
    if   n != "_cilk_ws" && n != "_cilk_frame"
    then
      -- _cilk_frame->scopeX.n = n;
      exprStmt(
        eqExpr(
          memberExpr(
            memberExpr(
              declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
              true,
              scopeName,
              location=builtinLoc(MODULE_NAME)
            ),
            false,
            name(n, location=builtinLoc(MODULE_NAME)),
            location=builtinLoc(MODULE_NAME)
          ),
          declRefExpr(name(n, location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
          location=builtinLoc(MODULE_NAME)
        )
      )
    else nullStmt();
}

{- based on cilkc2c/transform.c:MakeImportDecl() -}
function makeImportFunction
Decl ::= fname::Name body::Stmt
{
  local storage :: StorageClasses = foldStorageClass([staticStorageClass()]);
  local fnquals :: SpecialSpecifiers = nilSpecialSpecifier();
  local bty :: BaseTypeExpr = directTypeExpr(builtinType(nilQualifier(), voidType()));
  local importProcName :: Name = name("_cilk_" ++ fname.name ++ "_import", location=builtinLoc(MODULE_NAME));
  local attrs :: Attributes = nilAttribute();
  local dcls :: Decls = nilDecl();

  local resultType :: TypeModifierExpr = baseTypeExpr();
  local mty :: TypeModifierExpr = functionTypeExprWithArgs(resultType, importFunctionArgs, false, nilQualifier());
  local importFunctionArgs :: Parameters =
    foldParameterDecl([
      parameterDecl(
        nilStorageClass(),
        typedefTypeExpr(nilQualifier(), name("CilkWorkerState", location=builtinLoc(MODULE_NAME))),
        pointerTypeExpr(foldQualifier([constQualifier(location=builtinLoc(MODULE_NAME))]), baseTypeExpr()),
        justName(name("_cilk_ws", location=builtinLoc(MODULE_NAME))),
        nilAttribute()
      ),
      parameterDecl(
        nilStorageClass(),
        directTypeExpr(builtinType(nilQualifier(), voidType())),
        pointerTypeExpr(nilQualifier(), baseTypeExpr()),
        justName(name("_cilk_procargs_v", location=builtinLoc(MODULE_NAME))),
        nilAttribute()
      )
    ]);

  local importDecl :: Decl =
    functionDeclaration(
      functionDecl(storage, fnquals, bty, mty, importProcName, attrs, dcls, body)
    );

  return
    decls(foldDecl([
      inCCode(),
      importDecl
    ]));
}

{- based on cilkc2c/transform.c:MakeImportBody() -}
function makeImportBody
Stmt ::= fname::Name args::Parameters returnsVoid::Boolean
{
  local wsCastVoid :: Expr =
    explicitCastExpr(
      typeName(
        directTypeExpr(builtinType(nilQualifier(), voidType())),
        baseTypeExpr()
      ),
      declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      location=builtinLoc(MODULE_NAME)
    );
  local procargsvCastVoid :: Expr =
    explicitCastExpr(
      typeName(
        directTypeExpr(builtinType(nilQualifier(), voidType())),
        baseTypeExpr()
      ),
      declRefExpr(name("_cilk_procargs_v", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      location=builtinLoc(MODULE_NAME)
    );

  local argsStructName :: Name = name("_cilk_" ++ fname.name ++ "_args", location=builtinLoc(MODULE_NAME));
  local procargsvCastStruct :: Expr =
    explicitCastExpr(
      typeName(
        tagReferenceTypeExpr(nilQualifier(), structSEU(), argsStructName),
        pointerTypeExpr(nilQualifier(), baseTypeExpr())
      ),
      declRefExpr(name("_cilk_procargs_v", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      location=builtinLoc(MODULE_NAME)
    );
  local procResult :: Expr =
    memberExpr(
      procargsvCastStruct,
      true,
      name("_cilk_proc_result", location=builtinLoc(MODULE_NAME)),
      location=builtinLoc(MODULE_NAME)
    );

  local fastCloneArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
      makeFastCloneArgs(args, procargsvCastStruct)
    );

  local callFastClone :: Expr =
    directCallExpr(
      fname,
      fastCloneArgs,
      location=builtinLoc(MODULE_NAME)
    );
  local assignResult :: Expr = eqExpr(procResult, callFastClone, location=builtinLoc(MODULE_NAME));

  -- don't assign result if return void
  local mAssignResult :: Expr =
    if   returnsVoid
    then callFastClone
    else assignResult;

  return
    foldStmt([
      -- cast as void to prevent unused arg warning??
      exprStmt(wsCastVoid),
      exprStmt(procargsvCastVoid),

      exprStmt(mAssignResult)
    ]);
}

function makeExportFunction
Decl ::= newName::Name bty::BaseTypeExpr retMty::TypeModifierExpr args::Parameters body::Stmt
{
  local storage :: StorageClasses = nilStorageClass();
  local fnquals :: SpecialSpecifiers = nilSpecialSpecifier();
  local exportProcName :: Name = name("mt_" ++ newName.name, location=builtinLoc(MODULE_NAME));
  local attrs :: Attributes = nilAttribute();
  local dcls :: Decls = nilDecl();
  local mty :: TypeModifierExpr = functionTypeExprWithArgs(retMty, exportFunctionArgs, false, nilQualifier());
  local exportFunctionArgs :: Parameters =
    consParameters(
      parameterDecl(
        nilStorageClass(),
        typedefTypeExpr(nilQualifier(), name("CilkContext", location=builtinLoc(MODULE_NAME))),
        pointerTypeExpr(foldQualifier([constQualifier(location=bogusLoc())]), baseTypeExpr()),
        justName(name("context", location=builtinLoc(MODULE_NAME))),
        nilAttribute()
      ),
      args
    );

  local exportDecl :: Decl =
    functionDeclaration(
      functionDecl(storage, fnquals, bty, mty, exportProcName, attrs, dcls, body)
    );

  return
    decls(foldDecl([
      inCCode(),
      exportDecl
    ]));
}

function makeExportBody
Stmt ::= newName::Name resultType::BaseTypeExpr retMty::TypeModifierExpr args::Parameters
              returnsVoid::Boolean
{
  local procArgsName :: Name = name("_cilk_procargs", location=builtinLoc(MODULE_NAME));
  local procArgsStructName :: Name = name("_cilk_" ++ newName.name ++ "_args", location=builtinLoc(MODULE_NAME));
  local procArgsStruct :: BaseTypeExpr =
    tagReferenceTypeExpr(nilQualifier(), structSEU(), procArgsStructName);

  -- struct _cilk_foo_args *_cilk_procargs = ${initProcArgs};
  local procArgsDecl :: Stmt =
    declStmt(
      variableDecls(
        nilStorageClass(),
        nilAttribute(),
        tagReferenceTypeExpr(nilQualifier(), structSEU(), procArgsStructName),
        foldDeclarator([
          declarator(
            procArgsName,
            pointerTypeExpr(nilQualifier(), baseTypeExpr()),
            nilAttribute(),
            justInitializer(initProcArgs)
          )
        ])
      )
    );

  local sizeofArgs :: Expr =
    sizeofExpr(
      typeNameExpr(typeName(procArgsStruct, baseTypeExpr())),
      location=builtinLoc(MODULE_NAME)
    );

  -- (struct _cilk_foo_args *) Cilk_malloc_fixed(sizeof(struct _cilk_foo_args));
  local initProcArgs :: Initializer =
    exprInitializer(
      explicitCastExpr(
        typeName(
          procArgsStruct,
          pointerTypeExpr(nilQualifier(), baseTypeExpr())
        ),
        directCallExpr(
          name("Cilk_malloc_fixed", location=builtinLoc(MODULE_NAME)),
          foldExpr([sizeofArgs]),
          location=builtinLoc(MODULE_NAME)
        ),
        location=builtinLoc(MODULE_NAME)
      ),
      location=builtinLoc(MODULE_NAME)
    );

  -- Cilk_free(_cilk_procargs);
  local freeProcArgs :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_free", location=builtinLoc(MODULE_NAME)),
        foldExpr([
            declRefExpr(procArgsName, location=builtinLoc(MODULE_NAME))
        ]),
        location=builtinLoc(MODULE_NAME)
      )
    );

  local resultName :: Name = name("_cilk_proc_result", location=builtinLoc(MODULE_NAME));
  local resultDecl :: Stmt =
    if   returnsVoid
    then nullStmt()
    else
      declStmt(
        variableDecls(
          nilStorageClass(),
          nilAttribute(),
          resultType,
          foldDeclarator([
            declarator(
              resultName,
              retMty,
              nilAttribute(),
              justInitializer(initResult)
            )
          ])
        )
      );

  local initResult :: Initializer =
    exprInitializer(
      memberExpr(
        declRefExpr(procArgsName, location=builtinLoc(MODULE_NAME)),
        true,
        resultName,
        location=builtinLoc(MODULE_NAME)
      ),
      location=builtinLoc(MODULE_NAME)
    );

  local sizeofRet :: Expr =
    if   returnsVoid
    then mkIntConst(0, builtinLoc(MODULE_NAME))
    else
      sizeofExpr(
        typeNameExpr(typeName(resultType, baseTypeExpr())),
        location=builtinLoc(MODULE_NAME)
      );

  local importProcName :: Name = name("_cilk_" ++ newName.name ++ "_import", location=builtinLoc(MODULE_NAME));
  local cilkStart :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_start", location=builtinLoc(MODULE_NAME)),
        foldExpr([
            declRefExpr(name("context", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
            declRefExpr(importProcName, location=builtinLoc(MODULE_NAME)),
            declRefExpr(procArgsName, location=builtinLoc(MODULE_NAME)),
            sizeofRet
        ]),
        location=builtinLoc(MODULE_NAME)
      )
    );

  local returnResult :: Stmt =
    if   returnsVoid
    then nullStmt()
    else
      returnStmt(
        justExpr(
          declRefExpr(resultName, location=builtinLoc(MODULE_NAME))
        )
      );

  return
    foldStmt([
      procArgsDecl,
      initProcArgsFields(args, procArgsName),
      cilkStart,
      resultDecl,
      freeProcArgs,
      returnResult
    ]);
}

function initProcArgsFields
Stmt ::= args::Parameters procArgsName::Name
{
  return
    case args of
    | consParameters(h, t) -> seqStmt(
                                initProcArgsField(h, procArgsName),
                                initProcArgsFields(t, procArgsName)
                              )
    | nilParameters()      -> nullStmt()
    end;
}

function initProcArgsField
Stmt ::= arg::ParameterDecl procArgsName::Name
{
  local n :: Name =
    case arg.paramname of
    | just(n1) -> n1
    | _        -> error("cilk function parameter must be named")
    end;

  -- example: _cilk_procargs->x = x;
  return
    exprStmt(
      eqExpr(
        memberExpr(
          declRefExpr(procArgsName, location=builtinLoc(MODULE_NAME)),
          true, n, location=builtinLoc(MODULE_NAME)
        ),
        declRefExpr(n, location=builtinLoc(MODULE_NAME)),
        location=builtinLoc(MODULE_NAME)
      )
    );
}

function makeFastCloneArgs
Exprs ::= args::Parameters procargsv::Expr
{
  return
    case args of
    | consParameters(h, t) -> consExpr(makeFastCloneArg(h, procargsv), makeFastCloneArgs(t, procargsv))
    | nilParameters()      -> nilExpr()
    end;
}

function makeFastCloneArg
Expr ::= arg::ParameterDecl procargsv::Expr
{
  local n :: Name =
    case arg.paramname of
    | just(n1) -> n1
    | _        -> error("cilk function parameter must be named")
    end;

  return memberExpr(procargsv, true, n, location=builtinLoc(MODULE_NAME));
}

-- add CilkWorkerState*const _cilk_ws as the first parameter
function fastClone
Decl ::= bty::BaseTypeExpr  mty::Decorated TypeModifierExpr  newName::Name
         dcls::Decls  body::Stmt
{
  return
    decls(foldDecl([
      inFastProcedure(),

      -- The fast clone has the header
      --  `signed int fib(CilkWorkerState  *const  _cilk_ws, signed int  n)`
      functionDeclaration(
        functionDecl(nilStorageClass(), nilSpecialSpecifier(), bty, addWsToParams(mty), newName, nilAttribute(), dcls, body)
        )
    ]));
}

function addWsToParams
TypeModifierExpr ::= mty::Decorated TypeModifierExpr
{
  local wsParam :: ParameterDecl =
    parameterDecl(
      nilStorageClass(),
      typedefTypeExpr(nilQualifier(), name("CilkWorkerState", location=loc("ToDo",-10,-1,-1,-1,-1,-1))),
      pointerTypeExpr(foldQualifier([constQualifier(location=bogusLoc())]), baseTypeExpr()),
      justName(name( "_cilk_ws", location=loc("ToDo",-11,-1,-1,-1,-1,-1))),
      nilAttribute()
    );

  return
    case mty of
    | functionTypeExprWithArgs(ret, args, variadic, q) ->
        functionTypeExprWithArgs(ret, consParameters(wsParam, args), variadic, q)
    | functionTypeExprWithoutArgs(ret, ids, q) ->
        functionTypeExprWithArgs(ret, consParameters(wsParam, nilParameters()), false, q)
    | _ -> error("ToDo: fix this in Cilk ext.  Violating some rules about extensibility.")
    end;

}

abstract production transformFastClone
top::Stmt ::= body::Stmt newName::Name args::Parameters
{
  top.pp = text("cilkTransformFastClone()"); -- TODO: better pp
  top.functionDefs := fastClone.functionDefs;
  top.labelDefs := fastClone.labelDefs;


  local fastClone :: Stmt =
    foldStmt([
      addFastStuff(newName),
      body
    ]);

  fastClone.controlStmtContext = top.controlStmtContext;
  fastClone.env =
        addEnv(
          [
            miscDef(cilk_in_fast_clone_id, emptyMiscItem()),
            syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
          ],
          top.env
        );

  -- get all name/scopeIds pairs except those at global scope
  local cilkFrameVars :: [Pair<String String>] =
    foldr(append, [], map(tm:toList, take(length(top.env.scopeIds)-1, top.env.scopeIds)));

  local fwd :: Stmt =
    if   frameContainsShadow(cilkFrameVars)
    then warnStmt([err(builtinLoc(MODULE_NAME), "shadowing variable names in cilk functions is currently not supported")])
    else decStmt(fastClone);

  forwards to
    seqStmt(
      declStmt(
        defsDecl(
          [
            miscDef(cilk_in_fast_clone_id, emptyMiscItem()),
            syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
          ])),
      fwd);
}

function addFastStuff
Stmt ::= newName::Name
{
  local frameStructName :: Name = name("_cilk_" ++ newName.name ++ "_frame", location=builtinLoc(MODULE_NAME));
  local sigName :: Name = name("_cilk_" ++ newName.name ++ "_sig", location=builtinLoc(MODULE_NAME));
  local frameName :: Name = name("_cilk_frame", location=builtinLoc(MODULE_NAME));
  local ws :: Expr = declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME));

  -- declare _cilk_frame and expand CILK2C_INIT_FRAME() macro
  local frameDecl :: Stmt =
    foldStmt([
      exprStmt(comment("declare _cilk_frame and expand CILK2C_INIT_FRAME() macro", location=builtinLoc(MODULE_NAME))),
      declStmt(
        variableDecls(
          nilStorageClass(),
          nilAttribute(),
          tagReferenceTypeExpr(nilQualifier(), structSEU(), frameStructName),
          foldDeclarator([
            declarator(
              frameName,
              pointerTypeExpr(nilQualifier(), baseTypeExpr()),
              nilAttribute(),
              justInitializer(initFrame)
            )
          ])
        )
      )
    ]);

  local initFrame :: Initializer =
    exprInitializer(
      directCallExpr(
        name("Cilk_cilk2c_init_frame", location=builtinLoc(MODULE_NAME)),
        foldExpr([
          ws,
          sizeofExpr(
            typeNameExpr(
              typeName(
                tagReferenceTypeExpr(nilQualifier(), structSEU(), frameStructName),
                baseTypeExpr()
              )
            ),
            location=builtinLoc(MODULE_NAME)
          ),
          declRefExpr(sigName, location=builtinLoc(MODULE_NAME))
        ]),
        location=builtinLoc(MODULE_NAME)
      ),
      location=builtinLoc(MODULE_NAME)
    );

  -- expand CILK2C_START_THREAD_FAST() macro
  local startThreadFastCp :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_START_THREAD_FAST() macro", location=builtinLoc(MODULE_NAME))),
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_start_thread_fast_cp", location=builtinLoc(MODULE_NAME)),
          foldExpr([
            ws,
            mkAddressOf(
              memberExpr(
                declRefExpr(frameName, location=builtinLoc(MODULE_NAME)),
                true,
                name("header", location=builtinLoc(MODULE_NAME)),
                location=builtinLoc(MODULE_NAME)
              ),
              builtinLoc(MODULE_NAME)
            )
          ]),
          location=builtinLoc(MODULE_NAME)
        )
      )
    ]);

  local eventNewThreadMaybe :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_cilk2c_event_new_thread_maybe", location=builtinLoc(MODULE_NAME)),
        foldExpr([ws]),
        location=builtinLoc(MODULE_NAME)
      )
    );

  return
    foldStmt([
      frameDecl,
      startThreadFastCp,
      eventNewThreadMaybe
    ]);
}

function frameContainsShadow
Boolean ::= cilkFrameVars::[Pair<String String>]
{
  return
    if   null(cilkFrameVars)
    then false
    else contains(head(cilkFrameVars), tail(cilkFrameVars))
           || frameContainsShadow(tail(cilkFrameVars));
}

function pairFstStringEq
Boolean ::= l::Pair<String a> r::Pair<String a>
{
  return fst(l) == fst(r);
}

function slowClone
Decl ::= newName::Name dcls::Decls body::Stmt
{
  local slowName :: Name = name("_cilk_" ++ newName.name ++ "_slow", location=builtinLoc(MODULE_NAME));
  local void :: BaseTypeExpr = directTypeExpr(builtinType(nilQualifier(), voidType()));

  local newParams :: TypeModifierExpr =
    functionTypeExprWithArgs(baseTypeExpr(), mkSlowParams(newName), false, nilQualifier());

  return
    decls(foldDecl([
      inSlowProcedure(),

      -- The fast clone has the header
      --  `signed int fib(CilkWorkerState  *const  _cilk_ws, signed int  n)`
      functionDeclaration(
        functionDecl(foldStorageClass([staticStorageClass()]), nilSpecialSpecifier(), void, newParams, slowName, nilAttribute(), dcls, body)
        )
    ]));
}

function mkSlowParams
Parameters ::= newName::Name
{
  local frameStructName :: Name = name("_cilk_" ++ newName.name ++ "_frame", location=builtinLoc(MODULE_NAME));

  local wsParam :: ParameterDecl =
    parameterDecl(
      nilStorageClass(),
      typedefTypeExpr(nilQualifier(), name("CilkWorkerState", location=loc("ToDo",-10,-1,-1,-1,-1,-1))),
      pointerTypeExpr(foldQualifier([constQualifier(location=bogusLoc())]), baseTypeExpr()),
      justName(name( "_cilk_ws", location=loc("ToDo",-11,-1,-1,-1,-1,-1))),
      nilAttribute()
    );
  local frame :: ParameterDecl =
    parameterDecl(
      nilStorageClass(),
      tagReferenceTypeExpr(nilQualifier(), structSEU(), frameStructName),
      pointerTypeExpr(nilQualifier(), baseTypeExpr()),
      justName(name("_cilk_frame", location=builtinLoc(MODULE_NAME))),
      nilAttribute()
    );

  return foldParameterDecl([wsParam, frame]);
}

abstract production transformSlowClone
top::Stmt ::= body::Stmt args::Parameters
{
  top.pp = text("cilkTransformSlowClone()"); -- TODO: better pp

  top.functionDefs := body.functionDefs;
  top.labelDefs := body.labelDefs;

  body.controlStmtContext = top.controlStmtContext;
  body.env =
        addEnv(
          argDecls.defs ++
          [
            miscDef(cilk_in_slow_clone_id, emptyMiscItem()),
            syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
          ],
          top.env
        );

  args.position = 0;
  local argDecls :: Stmt = makeArgDecls(args);
  argDecls.env = top.env;
  argDecls.controlStmtContext = top.controlStmtContext;

  -- expand CILK2C_START_THREAD_SLOW() macro
  local startThreadSlow :: Stmt =
    foldStmt([
      exprStmt(comment("expand CILK2C_START_THREAD_SLOW() macro", location=builtinLoc(MODULE_NAME))),

      -- Cilk_cilk2c_start_thread_slow_cp(_cilk_ws, &(_cilk_frame->header));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_start_thread_slow_cp", location=builtinLoc(MODULE_NAME)),
          foldExpr([
            declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
                true,
                name("header", location=builtinLoc(MODULE_NAME)),
                location=builtinLoc(MODULE_NAME)
              ),
              builtinLoc(MODULE_NAME)
            )
          ]),
          location=builtinLoc(MODULE_NAME)
        )
      ),
      -- Cilk_cilk2c_start_thread_slow(_cilk_ws, &(_cilk_frame->header));
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_start_thread_slow", location=builtinLoc(MODULE_NAME)),
          foldExpr([
            declRefExpr(name("_cilk_ws", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
            mkAddressOf(
              memberExpr(
                declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
                true,
                name("header", location=builtinLoc(MODULE_NAME)),
                location=builtinLoc(MODULE_NAME)
              ),
              builtinLoc(MODULE_NAME)
            )
          ]),
          location=builtinLoc(MODULE_NAME)
        )
      )
    ]);

  -- switch (_cilk_frame->header.entry) { ... }
  -- TODO: don't use txtStmt
  local switchHeaderEntry :: Stmt =
    txtStmt("switch (_cilk_frame->header.entry) {"
    ++ makeSwitchHeaderCases(length(top.syncLocations)) ++ "}");
--    switchStmt(
--      memberExpr(
--        memberExpr(
--          declRefExpr(name("_cilk_frame", location=builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)),
--          true,
--          name("header", location=builtinLoc(MODULE_NAME)),
--          location=builtinLoc(MODULE_NAME)
--        ),
--        false,
--        name("entry", location=builtinLoc(MODULE_NAME)),
--        location=builtinLoc(MODULE_NAME)
--      ),
--      makeSwitchHeaderCases(length(top.syncLocations))
--    );

  forwards to
    foldStmt([
      declStmt(
        defsDecl([
          miscDef(cilk_in_slow_clone_id, emptyMiscItem()),
          syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
        ])),
      argDecls,
      startThreadSlow,
      switchHeaderEntry,
      restoreVariables(args.env),
      decStmt(body)
    ]);
}

--function makeSwitchHeaderCases
--Stmt ::= syncCount::Integer
--{
--  return
--    if   syncCount < 1
--    then nullStmt()
--    else
--      seqStmt(
--        caseLabelStmt(
--            mkIntConst(syncCount, builtinLoc(MODULE_NAME)),
--            gotoStmt(name("_cilk_sync" ++ toString(syncCount), location=builtinLoc(MODULE_NAME)))
--        ),
--        makeSwitchHeaderCases(syncCount - 1)
--      );
--}
function makeSwitchHeaderCases
String ::= syncCount::Integer
{
  return
    if   syncCount < 1
    then ""
    else "case " ++ toString(syncCount) ++ ": goto _cilk_sync" ++
      toString(syncCount) ++ "; " ++ makeSwitchHeaderCases(syncCount - 1);
}


{- based on cilkc2c/transform.c:MakeLinkage() -}
function makeLinkage
Decl ::= fname::Name bty::BaseTypeExpr bodyLinkage::[Init] returnsVoid::Boolean
{
  local sizeofRet :: Expr =
    if   returnsVoid
    then mkIntConst(0, builtinLoc(MODULE_NAME))
    else
      sizeofExpr(
        typeNameExpr(typeName(bty, baseTypeExpr())),
        location=builtinLoc(MODULE_NAME)
      );

  local frameStructName :: Name = name("_cilk_" ++ fname.name ++ "_frame", location=builtinLoc(MODULE_NAME));
  local sizeofFrame :: Expr =
    sizeofExpr(
      typeNameExpr(
        typeName(
          tagReferenceTypeExpr(nilQualifier(), structSEU(), frameStructName),
          baseTypeExpr()
        )
      ),
      location=builtinLoc(MODULE_NAME)
    );

  local slowCloneName :: Name = name("_cilk_" ++ fname.name ++ "_slow", location=builtinLoc(MODULE_NAME));
  local slowClone :: Expr = declRefExpr(slowCloneName, location=builtinLoc(MODULE_NAME));

  local initSig :: Initializer =
    objectInitializer(
      foldInit(
        cons(
          positionalInit(
            objectInitializer(
              foldInit([
                positionalInit(exprInitializer(sizeofRet, location=builtinLoc(MODULE_NAME))),
                positionalInit(exprInitializer(sizeofFrame, location=builtinLoc(MODULE_NAME))),
                positionalInit(exprInitializer(slowClone, location=builtinLoc(MODULE_NAME))),
                positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME))),
                positionalInit(exprInitializer(mkIntConst(0, builtinLoc(MODULE_NAME)), location=builtinLoc(MODULE_NAME)))
              ]),
              location=builtinLoc(MODULE_NAME)
            )
          ),
          reverse(bodyLinkage)
        )
      ),
      location=builtinLoc(MODULE_NAME)
    );

  return
    decls(foldDecl([
      inCCode(),
      variableDecls(
        foldStorageClass([staticStorageClass()]),
        nilAttribute(),
        typedefTypeExpr(nilQualifier(), name("CilkProcInfo", location=builtinLoc(MODULE_NAME))),
        foldDeclarator([
          declarator(
            name("_cilk_" ++ fname.name ++ "_sig", location=builtinLoc(MODULE_NAME)),
            arrayTypeExprWithoutExpr(baseTypeExpr(), nilQualifier(), normalArraySize()),
            nilAttribute(),
            justInitializer(initSig)
          )
        ])
      )
    ]));
}

function inFastProcedure
Decl ::=
{
  return makeWhereAmI("IN_FAST_PROCEDURE");
}

function inSlowProcedure
Decl ::=
{
  return makeWhereAmI("IN_SLOW_PROCEDURE");
}

function inCCode
Decl ::=
{
  return makeWhereAmI("IN_C_CODE");
}

function makeWhereAmI
Decl ::= s::String
{
  return
    txtDecl (s"""
#undef CILK_WHERE_AM_I
#define CILK_WHERE_AM_I ${s}
"""
    );
}

