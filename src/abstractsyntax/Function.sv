grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

aspect production functionDeclaration
top::Decl ::= f::FunctionDecl
{
  top.globalDecls <- [pair("CILK_WHERE_AM_I", inCCode())];
}

{- somewhat similar to cilkc2c/transform.c:TransformCilkProc() -}
abstract production cilkFunctionDecl
top::Decl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]
  bty::BaseTypeExpr mty::TypeModifierExpr  fname::Name  attrs::[Attribute]
  dcls::Decls  body::Stmt
{
  -- ToDo: check that storage, fnquals, and attrs are empty
      -- or just remove them?  I guess supporting them in concrete syntax
      -- could lead to nicer error messages than a parse error.

  top.pp = concat([
      terminate(space(), map((.pp), storage)),
      terminate( space(), map( (.pp), fnquals ) ),
      bty.pp, space(), mty.lpp, fname.pp, mty.rpp,
      ppAttributesRHS(attrs), line(),
      terminate(cat(semi(), line()), dcls.pps),
      text("{"), line(), nestlines(2,body.pp), text("}")
    ]);

  local cilkElision :: Decl =
    functionDeclaration(
      functionDecl( storage, fnquals, bty, mty, fname, attrs, dcls, body) ) ;

  local newName :: Name = case fname.name of
                          | "main" -> name("cilk_main", location=fname.location)
                          | _ -> fname
                          end;

  forwards to decls ( foldDecl ( newDecls ) );

  production attribute newDecls :: [Decl] with ++;
  newDecls := []; -- collection attributes contribute these declaration:e

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
  newDecls <- [frameStruct];
  local frameStruct :: Decl = makeFrame(newName, args, body);
--  body.scopesInh = args.scopes;
--  body.scopeCountInh = 0;

-- arg struct --------------------------------------------------
-- again, another syn attr or scope0 of the frame struct information
  newDecls <- [argStruct];
  local argStruct :: Decl = makeArgsAndResultStruct(newName, bty, args);

  local args :: Parameters =
    case mty of
    | functionTypeExprWithArgs(_, args1, _) -> args1
    | functionTypeExprWithoutArgs(_, _)     -> nilParameters()
    | _ -> error("ToDo: fix this in Cilk ext.  Violating some rules about extensibility.")
    end;

-- Slow Clone --------------------------------------------------

  -- `cilk_return;' is always necessary so add it just in case user didn't include it
  local newBody :: Stmt =
    if   returnsVoid
    then seqStmt(body, cilk_returnStmt(nothingExpr()))
    else body;

  local slowCloneDecl :: Decl = slowClone(newName, dcls, slowCloneBody);
  local slowCloneBody :: Stmt = transformSlowClone(newBody, args);
  newDecls <- [slowCloneDecl];

  slowCloneBody.env = top.env;
--  slowCloneBody.env = addEnv ([ miscDef(cilk_in_slow_clone_id, emptyMiscItem()) ], top.env);
--  slowCloneBody.scopeCountInh = 0;
  slowCloneBody.cilkLinksInh = [];
  slowCloneBody.returnType = nothing();
  slowCloneBody.cilkProcName = newName;

---- Proc Info --------------------------------------------------
  local linkage :: Decl = makeLinkage(newName, bty, slowCloneBody.cilkLinks, returnsVoid);
  newDecls <- [linkage];

-- Fast Clone --------------------------------------------------
  local fastCloneDecl :: Decl = fastClone(bty, mty, newName, dcls, fastCloneBody);
  local fastCloneBody :: Stmt = transformFastClone(newBody, newName, args);
  newDecls <- [fastCloneDecl];

-- Import Function --------------------------------------------------
  newDecls <- [ importDecl ];
  local importBody :: Stmt = makeImportBody(newName, args, returnsVoid);
  local importDecl :: Decl = makeImportFunction(newName, importBody);

  local returnsVoid :: Boolean =
    case bty of
      builtinTypeExpr(_, voidType()) -> true
    | _                              -> false
    end;

-- Export Function --------------------------------------------------
  newDecls <- [ exportDecl ];
  local exportBody :: Stmt = makeExportBody(newName, bty, args, returnsVoid);
  local exportDecl :: Decl = makeExportFunction(newName, bty, args, exportBody);
}

abstract production cilkFunctionProto
top::Decl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]
  bty::BaseTypeExpr mty::TypeModifierExpr  fname::Name  attrs::[Attribute]
{
  local slowName :: Name = name("_cilk_" ++ fname.name ++ "_slow", location=builtIn());
  local void :: BaseTypeExpr = directTypeExpr(builtinType([], voidType()));

  local slowMty :: TypeModifierExpr =
    functionTypeExprWithArgs(baseTypeExpr(), mkSlowParams(fname), false);

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
      [staticStorageClass()],
      [],
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
--  body.cilkFrameDeclsScopesInh = [];
--  body.cilkFrameDeclsScopesInh = cons([], args.cilkFrameDeclsScopes);
--  body.scopeCountInh = 0;

  local header :: StructItem =
    structItem(
      [],
      typedefTypeExpr([], name("CilkStackFrame", location=builtIn())),
      foldStructDeclarator([
        structField(name("header", location=builtIn()), baseTypeExpr(), [])
      ])
    );

  -- get all name/scopeIds pairs except those at global scope
--  local cilkFrameVars :: [Pair<String Integer>] =
--    foldr(append, [], map(tm:toList, take(length(body.env.scopeIds)-1, body.env.scopeIds)));

  -- collect all names with the same scopeId into a list and pair with the scopeId
--  local varsByScopes :: [Pair<Integer [String]>] = collectFrameVars(cilkFrameVars, []);

  -- TODO: if return type is not an arithmetic type, put it in frame
--  local frameFields :: StructItemList = makeFrameDeclsScopes(varsByScopes, body.env);

  -- collect all fields with the same scopeId into a list and pair with the scopeId
  local frameDeclsByScopes :: [Pair<String [StructItem]>] =
    collectFrameDecls(args.cilkFrameDeclsScopes ++ body.cilkFrameDeclsScopes, []);

  local frameFields :: [StructItem] =
    cons(header, map(makeFrameDeclsScope, frameDeclsByScopes));

--  local frameFields :: StructItemList =
--    consStructItem(
--      header,
--      consStructItem(
--        wrapFrameDeclsScopes(args.cilkFrameDeclsScopes, 0),
--        wrapFrameDeclsScopes(reverse(body.cilkFrameDeclsScopes), 0)
--      )
--    );

  forwards to
    typeExprDecl([],
      structTypeExpr(
        [],
        structDecl(
          [],
          justName(name("_cilk_" ++ newName.name ++ "_frame", location=builtIn())),
          foldStructItem(frameFields),
          location=builtIn()
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
      [],
      structTypeExpr(
        [],
        structDecl(
          [],
          nothingName(),
          foldStructItem(snd(cilkFrameDecl)),
          location=builtIn()
        )
      ),
      foldStructDeclarator([
        structField(
          name("scope" ++ fst(cilkFrameDecl), location=builtIn()),
          baseTypeExpr(),
          []
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
      [], -- TODO: attributes?
      value.typerep.baseTypeExpr,
      foldStructDeclarator([
        structField(name(n, location=builtIn()), value.typerep.typeModifierExpr, [])
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
                [],
                structTypeExpr(
                  [],
                  structDecl(
                    [],
                    nothingName(),
--                    nilStructItem(),
                    foldStructItem(head(cilkFrameDeclsScopes)),
                    location=builtIn()
                  )
                ),
                foldStructDeclarator([
                  structField(
                    name("scope" ++ toString(scopeCount), location=builtIn()),
                    baseTypeExpr(),
                    []
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
top::Decl ::= fname::Name  bty::BaseTypeExpr  args::Parameters
{
  local structName :: Name = name("_cilk_" ++ fname.name ++ "_args", location=builtIn());
  local resultField :: StructItem =
    structItem(
      [],
      bty,
      foldStructDeclarator([
        structField(name("_cilk_proc_result", location=builtIn()), baseTypeExpr(), [])
      ])
    );

  local argFields :: StructItemList = makeArgFields(args);

  local fields :: StructItemList =
    case bty.typerep of
    | builtinType(_, voidType()) -> argFields
    | _                          -> consStructItem(resultField, argFields)
    end;

  forwards to
    typeExprDecl([],
      structTypeExpr(
        [],
        structDecl([], justName(structName), fields, location=builtIn())
      )
    );
}

function makeArgFields
StructItemList ::= args::Parameters
{
  return
    case args of
    | consParameters(h, t) -> consStructItem(makeArgField(h), makeArgFields(t))
    | nilParameters()      -> nilStructItem()
    end;
}

{- FIXME: char *argv[] not supported, must be char **argv -}
function makeArgField
StructItem ::= arg::ParameterDecl
{
  local n :: Name =
    case arg.paramname of
    | just(n1) -> n1
    | _        -> error("cilk function parameter must be named")
    end;

  local attrs :: [Attribute] =
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
        structField(n, mty, [])
      ])
    );
}

{- TODO: is there any way to refactor this by converting Parameters to
    [ParameterDecl] then mapping a simpler function onto it? -}
function makeArgDecls
Stmt ::= args::Parameters
{
  return
    case args of
    | consParameters(h, t) -> seqStmt(makeArgDecl(h), makeArgDecls(t))
    | nilParameters()      -> nullStmt()
    end;
}

function makeArgDecl
Stmt ::= arg::ParameterDecl
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

  local attrs :: [Attribute] =
    case arg of parameterDecl(_, _, _, _, attrs1) -> attrs1 end;

  return
    declStmt(
      variableDecls(
        [], attrs, bty,
        foldDeclarator([ declarator(n, mty, [], nothingInitializer()) ])
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
      txtStmt("/* TODO: restore only live variables */"),
      foldStmt(map(restoreVariable, cilkFrameVars))
    ]);
}

function restoreVariable
Stmt ::= cilkFrameVar::Pair<String String>
{
  local n :: String = fst(cilkFrameVar);
  local scopeId :: String = snd(cilkFrameVar);
  return
    -- TODO: avoid putting _cilk_ws/frame into defs instead of handling as special case
    if   n != "_cilk_ws" && n != "_cilk_frame"
    then txtStmt(n ++ " = " ++ "_cilk_frame->scope" ++ scopeId ++ "." ++ n ++ ";")
    else nullStmt();
}

function saveVariables
Stmt ::= env::Decorated Env
{
  -- get all name/scopeIds pairs except those at global scope
  local cilkFrameVars :: [Pair<String String>] =
    foldr(append, [], map(tm:toList, take(length(env.scopeIds)-1, env.scopeIds)));

  return
    foldStmt([
      txtStmt("/* TODO: save only live, dirty variables */"),
      foldStmt(map(saveVariable, cilkFrameVars))
    ]);
}

function saveVariable
Stmt ::= cilkFrameVar::Pair<String String>
{
  local n :: String = fst(cilkFrameVar);
  local scopeId :: String = snd(cilkFrameVar);
  return
    -- TODO: avoid putting _cilk_ws/frame into defs instead of handling as special case
    if   n != "_cilk_ws" && n != "_cilk_frame"
    then txtStmt("_cilk_frame->scope" ++ scopeId ++ "." ++ n ++ " = "
           ++ n ++ ";")
    else nullStmt();
}

{- based on cilkc2c/transform.c:MakeImportDecl() -}
abstract production makeImportFunction
top::Decl ::= fname::Name body::Stmt
{
  local storage :: [StorageClass] = [staticStorageClass()];
  local fnquals :: [SpecialSpecifier] = [];
  local bty :: BaseTypeExpr = directTypeExpr(builtinType([], voidType()));
  local importProcName :: Name = name("_cilk_" ++ fname.name ++ "_import", location=builtIn());
  local attrs :: [Attribute] = [];
  local dcls :: Decls = nilDecl();

  local resultType :: TypeModifierExpr = baseTypeExpr();
  local mty :: TypeModifierExpr = functionTypeExprWithArgs(resultType, importFunctionArgs, false);
  local importFunctionArgs :: Parameters =
    foldParameterDecl([
      parameterDecl(
        [],
        typedefTypeExpr([], name("CilkWorkerState", location=builtIn())),
        pointerTypeExpr([constQualifier()], baseTypeExpr()),
        justName(name("_cilk_ws", location=builtIn())),
        []
      ),
      parameterDecl(
        [],
        directTypeExpr(builtinType([], voidType())),
        pointerTypeExpr([], baseTypeExpr()),
        justName(name("_cilk_procargs_v", location=builtIn())),
        []
      )
    ]);

  local importDecl :: Decl =
    functionDeclaration(
      functionDecl(storage, fnquals, bty, mty, importProcName, attrs, dcls, body)
    );

  forwards to
    decls(foldDecl([
      inCCode(),
      importDecl
    ]));
}

{- based on cilkc2c/transform.c:MakeImportBody() -}
abstract production makeImportBody
top::Stmt ::= fname::Name args::Parameters returnsVoid::Boolean
{
  local wsCastVoid :: Expr =
    explicitCastExpr(
      typeName(
        directTypeExpr(builtinType([], voidType())),
        baseTypeExpr()
      ),
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      location=builtIn()
    );
  local procargsvCastVoid :: Expr =
    explicitCastExpr(
      typeName(
        directTypeExpr(builtinType([], voidType())),
        baseTypeExpr()
      ),
      declRefExpr(name("_cilk_procargs_v", location=builtIn()), location=builtIn()),
      location=builtIn()
    );

  local argsStructName :: Name = name("_cilk_" ++ fname.name ++ "_args", location=builtIn());
  local procargsvCastStruct :: Expr =
    explicitCastExpr(
      typeName(
        tagReferenceTypeExpr([], structSEU(), argsStructName),
        pointerTypeExpr([], baseTypeExpr())
      ),
      declRefExpr(name("_cilk_procargs_v", location=builtIn()), location=builtIn()),
      location=builtIn()
    );
  local procResult :: Expr =
    memberExpr(
      procargsvCastStruct,
      true,
      name("_cilk_proc_result", location=builtIn()),
      location=builtIn()
    );

  local fastCloneArgs :: Exprs =
    consExpr(
      declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn()),
      makeFastCloneArgs(args, procargsvCastStruct)
    );

  local callFastClone :: Expr =
    directCallExpr(
      fname,
      fastCloneArgs,
      location=builtIn()
    );
  local assignResult :: Expr =
    binaryOpExpr(
      procResult,
      assignOp(eqOp(location=builtIn()), location=builtIn()),
      callFastClone,
      location=builtIn()
    );

  -- don't assign result if return void
  local mAssignResult :: Expr =
    if   returnsVoid
    then callFastClone
    else assignResult;

  forwards to
    foldStmt([
      -- cast as void to prevent unused arg warning??
      exprStmt(wsCastVoid),
      exprStmt(procargsvCastVoid),

      exprStmt(mAssignResult)
    ]);
}

abstract production makeExportFunction
top::Decl ::= newName::Name bty::BaseTypeExpr args::Parameters body::Stmt
{
  local storage :: [StorageClass] = [];
  local fnquals :: [SpecialSpecifier] = [];
  local exportProcName :: Name = name("mt_" ++ newName.name, location=builtIn());
  local attrs :: [Attribute] = [];
  local dcls :: Decls = nilDecl();
  local resultType :: TypeModifierExpr = baseTypeExpr();
  local mty :: TypeModifierExpr = functionTypeExprWithArgs(resultType, exportFunctionArgs, false);
  local exportFunctionArgs :: Parameters =
    consParameters(
      parameterDecl(
        [],
        typedefTypeExpr([], name("CilkContext", location=builtIn())),
        pointerTypeExpr([constQualifier()], baseTypeExpr()),
        justName(name("context", location=builtIn())),
        []
      ),
      args
    );

  local exportDecl :: Decl =
    functionDeclaration(
      functionDecl(storage, fnquals, bty, mty, exportProcName, attrs, dcls, body)
    );

  forwards to
    decls(foldDecl([
      inCCode(),
      exportDecl
    ]));
}

abstract production makeExportBody
top::Stmt ::= newName::Name resultType::BaseTypeExpr args::Parameters
              returnsVoid::Boolean
{
  local procArgsName :: Name = name("_cilk_procargs", location=builtIn());
  local procArgsStructName :: Name = name("_cilk_" ++ newName.name ++ "_args", location=builtIn());
  local procArgsStruct :: BaseTypeExpr =
    tagReferenceTypeExpr([], structSEU(), procArgsStructName);

  -- struct _cilk_foo_args *_cilk_procargs = ${initProcArgs};
  local procArgsDecl :: Stmt =
    declStmt(
      variableDecls(
        [],
        [],
        tagReferenceTypeExpr([], structSEU(), procArgsStructName),
        foldDeclarator([
          declarator(
            procArgsName,
            pointerTypeExpr([], baseTypeExpr()),
            [],
            justInitializer(initProcArgs)
          )
        ])
      )
    );

  local sizeofArgs :: Expr =
    unaryExprOrTypeTraitExpr(
      sizeofOp(location=builtIn()),
      typeNameExpr(typeName(procArgsStruct, baseTypeExpr())),
      location=builtIn()
    );

  -- (struct _cilk_foo_args *) Cilk_malloc_fixed(sizeof(struct _cilk_foo_args));
  local initProcArgs :: Initializer =
    exprInitializer(
      explicitCastExpr(
        typeName(
          procArgsStruct,
          pointerTypeExpr([], baseTypeExpr())
        ),
        directCallExpr(
          name("Cilk_malloc_fixed", location=builtIn()),
          foldExpr([sizeofArgs]),
          location=builtIn()
        ),
        location=builtIn()
      )
    );

  -- Cilk_free(_cilk_procargs);
  local freeProcArgs :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_free", location=builtIn()),
        foldExpr([
            declRefExpr(procArgsName, location=builtIn())
        ]),
        location=builtIn()
      )
    );

  local resultName :: Name = name("_cilk_proc_result", location=builtIn());
  local resultDecl :: Stmt =
    if   returnsVoid
    then nullStmt()
    else
      declStmt(
        variableDecls(
          [],
          [],
          resultType,
          foldDeclarator([
            declarator(
              resultName,
              baseTypeExpr(),
              [],
              justInitializer(initResult)
            )
          ])
        )
      );

  local initResult :: Initializer =
    exprInitializer(
      memberExpr(
        declRefExpr(procArgsName, location=builtIn()),
        true,
        resultName,
        location=builtIn()
      )
    );

  local sizeofRet :: Expr =
    if   returnsVoid
    then mkIntConst(0, builtIn())
    else
      unaryExprOrTypeTraitExpr(
        sizeofOp(location=builtIn()),
        typeNameExpr(typeName(resultType, baseTypeExpr())),
        location=builtIn()
      );

  local importProcName :: Name = name("_cilk_" ++ newName.name ++ "_import", location=builtIn());
  local cilkStart :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_start", location=builtIn()),
        foldExpr([
            declRefExpr(name("context", location=builtIn()), location=builtIn()),
            declRefExpr(importProcName, location=builtIn()),
            declRefExpr(procArgsName, location=builtIn()),
            sizeofRet
        ]),
        location=builtIn()
      )
    );

  local returnResult :: Stmt =
    if   returnsVoid
    then nullStmt()
    else
      returnStmt(
        justExpr(
          declRefExpr(resultName, location=builtIn())
        )
      );

  forwards to
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
      binaryOpExpr(
        memberExpr(
          declRefExpr(procArgsName, location=builtIn()),
          true, n, location=builtIn()
        ),
        assignOp(eqOp(location=builtIn()), location=builtIn()),
        declRefExpr(n, location=builtIn()),
        location=builtIn()
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

  return memberExpr(procargsv, true, n, location=builtIn());
}

-- add CilkWorkerState*const _cilk_ws as the first parameter
abstract production fastClone
d::Decl ::= bty::BaseTypeExpr mty::TypeModifierExpr newName::Name
  dcls::Decls  body::Stmt
{
  forwards to
    decls(foldDecl([
      inFastProcedure(),

      -- The fast clone has the header
      --  `signed int fib(CilkWorkerState  *const  _cilk_ws, signed int  n)`
      functionDeclaration(
        functionDecl([], [], bty, addWsToParams(mty), newName, [], dcls, body)
        )
    ]));
}

abstract production addWsToParams
top::TypeModifierExpr ::= mty::TypeModifierExpr
{
  local wsParam :: ParameterDecl =
    parameterDecl(
      [],
      typedefTypeExpr([], name("CilkWorkerState", location=loc("ToDo",-10,-1,-1,-1,-1,-1))),
      pointerTypeExpr([constQualifier()], baseTypeExpr()),
      justName(name( "_cilk_ws", location=loc("ToDo",-11,-1,-1,-1,-1,-1))),
      []
    );

  forwards to
    case mty of
    | functionTypeExprWithArgs(ret, args, variadic) ->
        functionTypeExprWithArgs(
          ret,
          consParameters(wsParam, args),
          variadic
        )
    | functionTypeExprWithoutArgs(ret, ids) ->
        functionTypeExprWithArgs(
          ret,
          consParameters(wsParam, nilParameters()),
          false
        )
    | _ -> error("ToDo: fix this in Cilk ext.  Violating some rules about extensibility.")
    end;

}

abstract production transformFastClone
top::Stmt ::= body::Stmt newName::Name args::Parameters
{
  top.globalDecls := [];
  top.defs := [];
--  top.defs := args.defs;
  top.freeVariables = [];
  top.functiondefs := [];

  local fastClone :: Stmt =
    foldStmt([
      addFastStuff(newName),
      body
    ]);

--  top.defs <- fastClone.defs;

--  fastClone.scopeCountInh = top.scopeCountInh;
--  fastClone.env =
--    addEnv(
--      [
--        miscDef(cilk_in_fast_clone_id, emptyMiscItem())
--      ],
--      top.env
--    );
  fastClone.returnType = body.returnType;

  -- get all name/scopeIds pairs except those at global scope
  local cilkFrameVars :: [Pair<String String>] =
    foldr(append, [], map(tm:toList, take(length(top.env.scopeIds)-1, top.env.scopeIds)));

  -- TODO: warn if any shadowed variables in cilk frame
--  local fwd :: Stmt = fastClone;
  local fwd :: Stmt =
    if   frameContainsShadow(cilkFrameVars)
    then warnStmt([err(builtIn(), "shadowing variable names in cilk functions is currently not supported")])
    else fastClone;

  forwards to
    fwd
    with {
      env =
        addEnv(
          [
            miscDef(cilk_in_fast_clone_id, emptyMiscItem()),
            syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
          ],
          top.env
        );
    };
}

abstract production addFastStuff
top::Stmt ::= newName::Name
{
  local frameStructName :: Name = name("_cilk_" ++ newName.name ++ "_frame", location=builtIn());
  local sigName :: Name = name("_cilk_" ++ newName.name ++ "_sig", location=builtIn());
  local frameName :: Name = name("_cilk_frame", location=builtIn());
  local ws :: Expr = declRefExpr(name("_cilk_ws", location=builtIn()), location=builtIn());

  -- declare _cilk_frame and expand CILK2C_INIT_FRAME() macro
  local frameDecl :: Stmt =
    foldStmt([
      txtStmt("/* declare _cilk_frame and expand CILK2C_INIT_FRAME() macro */"),
      declStmt(
        variableDecls(
          [],
          [],
          tagReferenceTypeExpr([], structSEU(), frameStructName),
          foldDeclarator([
            declarator(
              frameName,
              pointerTypeExpr([], baseTypeExpr()),
              [],
              justInitializer(initFrame)
            )
          ])
        )
      )
    ]);

  local initFrame :: Initializer =
    exprInitializer(
      directCallExpr(
        name("Cilk_cilk2c_init_frame", location=builtIn()),
        foldExpr([
          ws,
          unaryExprOrTypeTraitExpr(
            sizeofOp(location=builtIn()),
            typeNameExpr(
              typeName(
                tagReferenceTypeExpr([], structSEU(), frameStructName),
                baseTypeExpr()
              )
            ),
            location=builtIn()
          ),
          declRefExpr(sigName, location=builtIn())
        ]),
        location=builtIn()
      )
    );

  -- expand CILK2C_START_THREAD_FAST() macro
  local startThreadFastCp :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_START_THREAD_FAST() macro */"),
      exprStmt(
        directCallExpr(
          name("Cilk_cilk2c_start_thread_fast_cp", location=builtIn()),
          foldExpr([
            ws,
            mkAddressOf(
              memberExpr(
                declRefExpr(frameName, location=builtIn()),
                true,
                name("header", location=builtIn()),
                location=builtIn()
              ),
              builtIn()
            )
          ]),
          location=builtIn()
        )
      )
    ]);

  local eventNewThreadMaybe :: Stmt =
    exprStmt(
      directCallExpr(
        name("Cilk_cilk2c_event_new_thread_maybe", location=builtIn()),
        foldExpr([ws]),
        location=builtIn()
      )
    );

  forwards to
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
    else containsBy(pairFstStringEq, head(cilkFrameVars), tail(cilkFrameVars))
           || frameContainsShadow(tail(cilkFrameVars));
}

function pairFstStringEq
Boolean ::= l::Pair<String a> r::Pair<String a>
{
  return fst(l) == fst(r);
}

abstract production slowClone
d::Decl ::= newName::Name dcls::Decls body::Stmt
{
  local slowName :: Name = name("_cilk_" ++ newName.name ++ "_slow", location=builtIn());
  local void :: BaseTypeExpr = directTypeExpr(builtinType([], voidType()));

  local newParams :: TypeModifierExpr =
    functionTypeExprWithArgs(baseTypeExpr(), mkSlowParams(newName), false);

  forwards to
    decls(foldDecl([
      inSlowProcedure(),

      -- The fast clone has the header
      --  `signed int fib(CilkWorkerState  *const  _cilk_ws, signed int  n)`
      functionDeclaration(
        functionDecl([staticStorageClass()], [], void, newParams, slowName, [], dcls, body)
        )
    ]));
}

function mkSlowParams
Parameters ::= newName::Name
{
  local frameStructName :: Name = name("_cilk_" ++ newName.name ++ "_frame", location=builtIn());

  local wsParam :: ParameterDecl =
    parameterDecl(
      [],
      typedefTypeExpr([], name("CilkWorkerState", location=loc("ToDo",-10,-1,-1,-1,-1,-1))),
      pointerTypeExpr([constQualifier()], baseTypeExpr()),
      justName(name( "_cilk_ws", location=loc("ToDo",-11,-1,-1,-1,-1,-1))),
      []
    );
  local frame :: ParameterDecl =
    parameterDecl(
      [],
      tagReferenceTypeExpr([], structSEU(), frameStructName),
      pointerTypeExpr([], baseTypeExpr()),
      justName(name("_cilk_frame", location=builtIn())),
      []
    );

  return foldParameterDecl([wsParam, frame]);
}

abstract production transformSlowClone
top::Stmt ::= body::Stmt args::Parameters
{
  -- top.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  top.globalDecls := [];
  top.defs := [];
  top.freeVariables = [];
  top.functiondefs := [];

  local argDecls :: Stmt = makeArgDecls(args);
  argDecls.env = top.env;

  -- expand CILK2C_START_THREAD_SLOW() macro
  local startThreadSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_START_THREAD_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_start_thread_slow_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_start_thread_slow(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local switchHeaderEntry :: Stmt =
    txtStmt("switch (_cilk_frame->header.entry) {"
    ++ makeSwitchHeaderCases(length(top.syncLocations)) ++ "}");

  forwards to
    foldStmt([
      argDecls,
      startThreadSlow,
      switchHeaderEntry,
      restoreVariables(args.env),
      body
    ])
  with {
    env = addEnv([
        miscDef(cilk_in_slow_clone_id, emptyMiscItem()),
        syncLocationsDef(cilk_sync_locations_id, top.syncLocations)
      ],
      top.env);
  } ;
}

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
abstract production makeLinkage
top::Decl ::= fname::Name bty::BaseTypeExpr bodyLinkage::[Init] returnsVoid::Boolean
{
  local sizeofRet :: Expr =
    if   returnsVoid
    then mkIntConst(0, builtIn())
    else
      unaryExprOrTypeTraitExpr(
        sizeofOp(location=builtIn()),
        typeNameExpr(typeName(bty, baseTypeExpr())),
        location=builtIn()
      );

  local frameStructName :: Name = name("_cilk_" ++ fname.name ++ "_frame", location=builtIn());
  local sizeofFrame :: Expr =
    unaryExprOrTypeTraitExpr(
      sizeofOp(location=builtIn()),
      typeNameExpr(
        typeName(
          tagReferenceTypeExpr([], structSEU(), frameStructName),
          baseTypeExpr()
        )
      ),
      location=builtIn()
    );

  local slowCloneName :: Name = name("_cilk_" ++ fname.name ++ "_slow", location=builtIn());
  local slowClone :: Expr = declRefExpr(slowCloneName, location=builtIn());

  local initSig :: Initializer =
    objectInitializer(
      foldInit(
        cons(
          init(
            objectInitializer(
              foldInit([
                init(exprInitializer(sizeofRet)),
                init(exprInitializer(sizeofFrame)),
                init(exprInitializer(slowClone)),
                init(exprInitializer(mkIntConst(0, builtIn()))),
                init(exprInitializer(mkIntConst(0, builtIn())))
              ])
            )
          ),
          reverse(bodyLinkage)
        )
      )
    );

  forwards to
    decls(foldDecl([
      inCCode(),
      variableDecls(
        [staticStorageClass()],
        [],
        typedefTypeExpr([], name("CilkProcInfo", location=builtIn())),
        foldDeclarator([
          declarator(
            name("_cilk_" ++ fname.name ++ "_sig", location=builtIn()),
            arrayTypeExprWithoutExpr(baseTypeExpr(), [], normalArraySize()),
            [],
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

-- New location for expressions which don't have real locations
abstract production builtIn
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}

