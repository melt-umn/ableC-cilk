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
  local slowCloneDecl :: Decl = slowClone(newName, dcls, slowCloneBody);
  local slowCloneBody :: Stmt = transformSlowClone(body, newName, args);
  newDecls <- [slowCloneDecl];

---- Proc Info --------------------------------------------------
  local linkage :: Decl = makeLinkage(newName, bty);
  newDecls <- [linkage];

-- Fast Clone --------------------------------------------------
  local fastCloneDecl :: Decl = fastClone(bty, mty, newName, dcls, fastCloneBody);
  local fastCloneBody :: Stmt = transformFastClone(body, newName);
  newDecls <- [fastCloneDecl];

-- Import Function --------------------------------------------------
  newDecls <- [ importDecl ];
  local importBody :: Stmt = makeImportBody(newName, args);
  local importDecl :: Decl = makeImportFunction(newName, importBody);


-- Export Function --------------------------------------------------
  newDecls <- [ exportDecl ];
  local exportBody :: Stmt = makeExportBody(newName, bty, args);
  local exportDecl :: Decl = makeExportFunction(newName, bty, args, exportBody);
}

abstract production cilkFunctionProto
top::Decl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]
  bty::BaseTypeExpr mty::TypeModifierExpr  fname::Name  attrs::[Attribute]
  dcls::Decls
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
  local frameFields :: StructItemList =
    consStructItem(
      structItem(
        [],
        typedefTypeExpr([], name("CilkStackFrame", location=builtIn())),
        foldStructDeclarator([
          structField(name("header", location=builtIn()), baseTypeExpr(), [])
        ])
      ),
      scopes
    );

  local scopes :: StructItemList = consStructItem(scope0, findAllScopes(body));
--  local scopes :: StructItemList = consStructItem(scope0, nilStructItem());

  -- TODO: don't add scope0 if args are void
  -- TODO: if return type is not an arithmetic type, put it in frame
  local scope0 :: StructItem =
      structItem(
        [],
        structTypeExpr(
          [],
          structDecl([], nothingName(), makeArgFields(args), location=builtIn())
        ),
        foldStructDeclarator([
          structField(name("scope0", location=builtIn()), baseTypeExpr(), [])
        ])
      );

  forwards to
    typeExprDecl([],
      structTypeExpr(
        [],
        structDecl(
          [],
          justName(name("_cilk_" ++ newName.name ++ "_frame", location=builtIn())),
          frameFields,
          location=builtIn()
        )
      )
    );
}

abstract production findAllScopes
top::StructItemList ::= body::Stmt
{
  forwards to foldStructItem(fst(findAllScopes1(body, 0)));
}

function findAllScopes1
Pair<[StructItem] Integer> ::= body::Stmt n::Integer
{
  return
    case body of
    | declStmt(d) ->
        case d of
        | variableDecls(_, attrs, ty, dcls) ->
            pair(
              [
                structItem(
                  [],
                  structTypeExpr(
                    [],
                    structDecl(
                      [],
                      nothingName(),
                      foldStructItem([
                        structItem(attrs, ty, makeStructDecls(dcls))
                      ]),
                      location=builtIn()
                    )
                  ),
                  foldStructDeclarator([
                    structField(
                      name("scope" ++ toString(n), location=builtIn()),
                      baseTypeExpr(),
                      []
                    )
                  ])
                )
              ],
              n
            )
        | _                                 -> pair([], n)
        end
    -- TODO: add i decl to scope
    | forDeclStmt(i, _, _, b)     -> findAllScopes1(b, n+1)
    | seqStmt(h, t)               ->
        case findAllScopes1(h, n) of
        | pair(scopes1, n1) -> mergeScopes(scopes1, findAllScopes1(t, n1))
        end
    | compoundStmt(s)             -> findAllScopes1(s, n+1)
    | whileStmt(_, b)             -> findAllScopes1(b, n+1)
    | doStmt(b, _)                -> findAllScopes1(b, n+1)
    | forStmt(_, _, _, b)         -> findAllScopes1(b, n+1)
    | switchStmt(_, b)            -> findAllScopes1(b, n+1)
    | labelStmt(_, s)             -> findAllScopes1(s, n)
    | caseLabelStmt(_, s)         -> findAllScopes1(s, n)
    | defaultLabelStmt(s)         -> findAllScopes1(s, n)
    | caseLabelRangeStmt(_, _, s) -> findAllScopes1(s, n)
    -- TODO: any reason functionDeclStmt should be handled here?
--    | functionDeclStmt(d)    ->
    | ifStmt(_, t, e)             ->
        case findAllScopes1(t, n) of
        | pair(scopes1, n1) -> mergeScopes(scopes1, findAllScopes1(e, n1))
        end
    -- need to have explicit case for cilk_returnStmt, otherwise it will try to
    --  forward to returnStmt and run into problems with inherited attributes
    | cilk_returnStmt(_)          -> pair([], n)
    | cilk_syncStmt()             -> pair([], n)
    | cilk_exitStmt(_)            -> pair([], n)
    | cilkSpawnStmt(_, _, _, _)   -> pair([], n)
    | _                           -> pair([], n)
    end;
}

abstract production mergeScopes
top::Pair<[StructItem] Integer> ::= scopes1::[StructItem] p::Pair<[StructItem] Integer>
{
  forwards to
    case p of
      -- FIXME: this is not implemented correctly
      pair(scopes2, n) -> pair(scopes1 ++ scopes2, n)
    end;
}

function makeStructDecls
StructDeclarators ::= dcls::Declarators
{
  return
    case dcls of
    | consDeclarator(h, t) ->
        case h of
        | declarator(n, ty, attrs, _) ->
            consStructDeclarator(
              structField(n, ty, attrs),
              makeStructDecls(t)
            )
        | errorDeclarator(msg) ->
            -- TODO: improve this message
            error("errorDeclarator found")
        end
    | nilDeclarator()      ->
        nilStructDeclarator()
    end;
}

{- based on cilkc2c/transform.c:MakeArgsAndResultStruct()

   struct _cilk_foo_args {
     return_type _cilk_proc_result; // if foo return type is not void
     ... args ...
   };
-}
function makeArgsAndResultStruct
Decl ::= fname::Name  bty::BaseTypeExpr  args::Parameters
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

  return
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
top::Stmt ::= fname::Name args::Parameters
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
  -- TODO: don't assign result if return void
  local assignResult :: Expr =
    binaryOpExpr(
      procResult,
      assignOp(eqOp(location=builtIn()), location=builtIn()),
      callFastClone,
      location=builtIn()
    );

  forwards to
    foldStmt([
      -- cast as void to prevent unused arg warning??
      exprStmt(wsCastVoid),
      exprStmt(procargsvCastVoid),

      exprStmt(assignResult)
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

  -- TODO: don't declare result if return void
  local initResult :: Initializer =
    exprInitializer(
      memberExpr(
        declRefExpr(procArgsName, location=builtIn()),
        true,
        resultName,
        location=builtIn()
      )
    );

  -- TODO: set to 0 if return void
  local sizeofRet :: Expr =
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

  -- TODO: don't return result if return void
  local returnResult :: Stmt =
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
top::Stmt ::= body::Stmt newName::Name
{
  forwards to
    foldStmt([
      addFastStuff(newName),
      body
    ])
    with {
      env =
        addEnv(
          [
            miscDef(cilk_in_fast_clone_id, emptyMiscItem())
--            miscDef(cilk_new_proc_name, stringMiscItem(newName.name))
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

  local frameDecl :: Stmt =
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
    );

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

  local startThreadFastCp :: Stmt =
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
    );

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
top::Stmt ::= body::Stmt newName::Name args::Parameters
{
  -- top.env depends on these, if not set then compiler will crash while looping
  --  in forwarded stmt to look for these
  top.globalDecls := [];
  top.defs = [];
  top.freeVariables = [];
  top.functiondefs = [];

  local argDecls :: Stmt = makeArgDecls(args);

  -- expand CILK2C_START_THREAD_SLOW() macro
  local startThreadSlow :: Stmt =
    foldStmt([
      txtStmt("/* expand CILK2C_START_THREAD_SLOW() macro */"),
      txtStmt("Cilk_cilk2c_start_thread_slow_cp(_cilk_ws, &(_cilk_frame->header));"),
      txtStmt("Cilk_cilk2c_start_thread_slow(_cilk_ws, &(_cilk_frame->header));")
    ]);

  local switchHeaderEntry :: Stmt =
    txtStmt("switch (_cilk_frame->header.entry) {" ++ makeSwitchHeaderCases(top.syncCount) ++ "}");

  forwards to
    foldStmt([
      argDecls,
      startThreadSlow,
      switchHeaderEntry,
      transformSlowStmt(body, newName)
      -- TODO: restore variables
    ])
  with { env = addEnv ([ miscDef(cilk_in_slow_clone_id, emptyMiscItem()) ], top.env); } ;
}

abstract production transformSlowStmt
top::Stmt ::= body::Stmt newName::Name
{
  forwards to
    foldStmt([
      case body of
      | nullStmt()      -> nullStmt()
      | seqStmt(h, t)   -> seqStmt(transformSlowStmt(h, newName), transformSlowStmt(t, newName))
      | compoundStmt(s) -> compoundStmt(transformSlowStmt(s, newName))
  --    | compoundStmt(s) -> txtStmt("/* compoundStmt */")
  ----    | declStmt(d)     ->
  ----    | basicVarDeclStmt(d) ->
  ----    | exprStmt(d)     ->
  --    | ifStmt(c, t, e) -> ifStmt(c, transformSlowStmt(t, newName), transformSlowStmt(e, newName))
  --    | _               -> txtStmt("/* transformSlowStmt not fully implemented yet */")
      | _               -> body
      end
    ]);
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
top::Decl ::= fname::Name bty::BaseTypeExpr
{
  -- TODO: set to 0 if return void
  local sizeofRet :: Expr =
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
      foldInit([
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
        )
      -- TODO: scope sigs will go here, but they need to be defined from spawn/sync
      ])
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

abstract production stringMiscItem
top::MiscItem ::= s::String
{
}

-- New location for expressions which don't have real locations
abstract production builtIn
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}

