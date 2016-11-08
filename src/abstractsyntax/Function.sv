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

-- Slow prototype --------------------------------------------------
-- Done.
  newDecls <- [slowPrototype];
  local slowPrototype :: Decl = 
    txtDecl (s"static void _cilk_${newName.name}_slow ( CilkWorkerState *const _cilk_ws, struct _cilk_${newName.name}_frame*_cilk_frame); ") ;

-- Proc Info --------------------------------------------------
  newDecls <- [cilkProcInfo];
  local cilkProcInfo :: Decl = case fname.name of
    | "main" ->
    txtDecl (s"""
static CilkProcInfo _cilk_cilk_main_sig[] = {
  { sizeof(int),sizeof(struct _cilk_cilk_main_frame),_cilk_cilk_main_slow,0,0},
  {0,0,0,0,0},
  {sizeof(int),CILK_OFFSETOF(struct _cilk_cilk_main_frame,scope1.result),0,0,0},
  {0,0,0,0,0}
 }; """ )

    | "fib" ->
    txtDecl (s"""
static CilkProcInfo _cilk_fib_sig[] = {
  { sizeof(int),sizeof(struct _cilk_fib_frame),_cilk_fib_slow,0,0 },
  { sizeof(int),CILK_OFFSETOF(struct _cilk_fib_frame,scope2.x),0,0,0 },
  { sizeof(int),CILK_OFFSETOF(struct _cilk_fib_frame,scope2.y),0,0,0 },
  { 0,0,0,0,0 }
 }; """ )

    | _ -> error ("non supported Cilk function: " ++ fname.name ++ "\n\n")
    end;



-- Fast Clone --------------------------------------------------
  newDecls <- [ fastCloneDecl ];
  local fastCloneDecl :: Decl = fastClone (bty, mty, newName, dcls, fastCloneBody) ;
  local fastCloneBody :: Stmt = case fname.name of
    | "main" ->
--#undef CILK_WHERE_AM_I
--#define CILK_WHERE_AM_I IN_FAST_PROCEDURE
--// # 43
--int cilk_main (CilkWorkerState *const _cilk_ws, int argc, char**argv) {

-- the following gets plugged in in the fastClone production
--  struct _cilk_cilk_main_frame*_cilk_frame;
--  CILK2C_INIT_FRAME(_cilk_frame,sizeof(struct _cilk_cilk_main_frame),_cilk_cilk_main_sig);
--  CILK2C_START_THREAD_FAST();

    txtStmt (s"""
  {
    int n;int result;

    if (argc != 2) {
      fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
      do { 
        { _cilk_frame->header.entry=1;
          _cilk_frame->scope0.argv=argv;
          CILK2C_BEFORE_SPAWN_FAST();
          CILK2C_PUSH_FRAME(_cilk_frame);
          Cilk_really_exit(_cilk_ws,1);
          CILK2C_XPOP_FRAME_NORESULT(_cilk_frame,0);
          CILK2C_AFTER_SPAWN_FAST();
        }
      }while (0);

    }
    n = atoi(argv[1]);
    { _cilk_frame->header.entry=2;
      CILK2C_BEFORE_SPAWN_FAST();
      CILK2C_PUSH_FRAME(_cilk_frame);
      result=fib(_cilk_ws,n);
      { int __tmp;
        CILK2C_XPOP_FRAME_RESULT(_cilk_frame,0,result);
      }
      CILK2C_AFTER_SPAWN_FAST();
    }
    CILK2C_AT_SYNC_FAST();

    printf("Result: %d\n", result);
    { int _cilk_temp4=0;
      CILK2C_BEFORE_RETURN_FAST();
      return _cilk_temp4;
    }
  } """ )
--}

    | "fib" ->
--#undef CILK_WHERE_AM_I
--#define CILK_WHERE_AM_I IN_FAST_PROCEDURE
--
--int fib     (CilkWorkerState*const _cilk_ws,int n) {

-- this below now inserted in fastClone production
--  struct _cilk_fib_frame*_cilk_frame;
--  CILK2C_INIT_FRAME(_cilk_frame,sizeof(struct _cilk_fib_frame),_cilk_fib_sig);
--  CILK2C_START_THREAD_FAST();


    txtStmt (s"""
  {
    if (n < 2) {
      int _cilk_temp0=(n);
      CILK2C_BEFORE_RETURN_FAST();
      return _cilk_temp0;
    }
    else {
      int x;
      int y;
      { _cilk_frame->header.entry=1;
        _cilk_frame->scope0.n=n;
        CILK2C_BEFORE_SPAWN_FAST();
        CILK2C_PUSH_FRAME(_cilk_frame);
        x=fib(_cilk_ws,n-1);
        { int __tmp;
          CILK2C_XPOP_FRAME_RESULT(_cilk_frame,0,x);
        }
        CILK2C_AFTER_SPAWN_FAST();
      }
      { _cilk_frame->header.entry=2;
        _cilk_frame->scope2.x=x;
        CILK2C_BEFORE_SPAWN_FAST();
        CILK2C_PUSH_FRAME(_cilk_frame);
        y=fib(_cilk_ws,n-2);
        { int __tmp; 
          CILK2C_XPOP_FRAME_RESULT(_cilk_frame,0,y);
        }
        CILK2C_AFTER_SPAWN_FAST();
      }
      CILK2C_AT_SYNC_FAST();
      { int _cilk_temp1=(x+y);
        CILK2C_BEFORE_RETURN_FAST();
        return _cilk_temp1;
      }
    }
} """ )
--  }

    | _ -> error ("non supported Cilk function: " ++ fname.name ++ "\n\n")
    end;



-- Slow Clone --------------------------------------------------
  newDecls <- [ slowClone ];
  local slowClone :: Decl = case fname.name of
    | "main" ->
    txtDecl (s"""
#undef CILK_WHERE_AM_I
#define CILK_WHERE_AM_I IN_SLOW_PROCEDURE
// # 43
static void _cilk_cilk_main_slow ( CilkWorkerState *const _cilk_ws, struct _cilk_cilk_main_frame *_cilk_frame) {
  int argc;
  char**argv;
  CILK2C_START_THREAD_SLOW();
  switch (_cilk_frame->header.entry) {
    case 1: goto _cilk_sync1;
    case 2: goto _cilk_sync2;
    case 3: goto _cilk_sync3;
  }
  argc=_cilk_frame->scope0.argc;
  argv=_cilk_frame->scope0.argv;
  {
    int n;

    if (argc != 2) {
      fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
      do { 
        { _cilk_frame->header.entry=1;
          _cilk_frame->scope0.argv=argv;
          CILK2C_BEFORE_SPAWN_SLOW();
          CILK2C_PUSH_FRAME(_cilk_frame);
          Cilk_really_exit(_cilk_ws,1);
          CILK2C_XPOP_FRAME_NORESULT(_cilk_frame,/* return nothing */);
          CILK2C_AFTER_SPAWN_SLOW();
          if (0) {
            _cilk_sync1 : argv=_cilk_frame->scope0.argv;
          }
          CILK2C_AT_THREAD_BOUNDARY_SLOW();
        }
      } while (0);
    }
    n = atoi(argv[1]);
    { int _cilk_temp5;
      _cilk_frame->header.entry=2;
      CILK2C_BEFORE_SPAWN_SLOW();
      CILK2C_PUSH_FRAME(_cilk_frame);
      _cilk_temp5=fib(_cilk_ws,n);
      { int __tmp;
        CILK2C_XPOP_FRAME_RESULT(_cilk_frame,/* return nothing */,_cilk_temp5);
      }
      CILK2C_AFTER_SPAWN_SLOW();
      _cilk_frame->scope1.result=_cilk_temp5;
      if (0) {
        _cilk_sync2:;
      }
      CILK2C_AT_THREAD_BOUNDARY_SLOW();
    }
    { CILK2C_BEFORE_SYNC_SLOW();
      _cilk_frame->header.entry=3;
      if (CILK2C_SYNC) {
        return;
        _cilk_sync3:;
      }
      CILK2C_AFTER_SYNC_SLOW();
      CILK2C_AT_THREAD_BOUNDARY_SLOW();
    }

    printf("Result: %d\n", _cilk_frame->scope1.result);
    { 
      { int __tmp=0;
        Cilk_set_result(_cilk_ws,&__tmp,sizeof((__tmp)));
      }
      CILK2C_BEFORE_RETURN_SLOW();
      return;
    }
  }
} """ )

    | "fib" ->
    txtDecl (s"""
#undef CILK_WHERE_AM_I
#define CILK_WHERE_AM_I IN_SLOW_PROCEDURE

static void _cilk_fib_slow ( CilkWorkerState *const _cilk_ws, struct _cilk_fib_frame *_cilk_frame) {  
  int n;
  CILK2C_START_THREAD_SLOW();
  switch (_cilk_frame->header.entry) 
  { case 1: goto _cilk_sync1;
    case 2: goto _cilk_sync2;
    case 3: goto _cilk_sync3;
  }
  n=_cilk_frame->scope0.n;
  {
    if (n < 2) {
      { int __tmp=(n);
        Cilk_set_result(_cilk_ws,&__tmp,sizeof((__tmp)));
      }
      CILK2C_BEFORE_RETURN_SLOW();
      return;
    }
    else {
      { int _cilk_temp2;
        _cilk_frame->header.entry=1;
        _cilk_frame->scope0.n=n;
        CILK2C_BEFORE_SPAWN_SLOW();
        CILK2C_PUSH_FRAME(_cilk_frame);
        _cilk_temp2=fib(_cilk_ws,n-1);
        { int __tmp;
          CILK2C_XPOP_FRAME_RESULT(_cilk_frame,/* return nothing */,_cilk_temp2);
        }
        CILK2C_AFTER_SPAWN_SLOW();
        _cilk_frame->scope2.x=_cilk_temp2;
        if (0) {
          _cilk_sync1: n = _cilk_frame->scope0.n;
        }
        CILK2C_AT_THREAD_BOUNDARY_SLOW();
      }
    { int _cilk_temp3;
      _cilk_frame->header.entry=2;
      CILK2C_BEFORE_SPAWN_SLOW();
      CILK2C_PUSH_FRAME(_cilk_frame);
      _cilk_temp3=fib(_cilk_ws,n-2);
      { int __tmp;
        CILK2C_XPOP_FRAME_RESULT(_cilk_frame,/* return nothing */,_cilk_temp3);
      }
      CILK2C_AFTER_SPAWN_SLOW();
      _cilk_frame->scope2.y=_cilk_temp3;
      if (0) {
        _cilk_sync2: ; 
      }
      CILK2C_AT_THREAD_BOUNDARY_SLOW();
    }

    { CILK2C_BEFORE_SYNC_SLOW();
      _cilk_frame->header.entry=3;
      if (CILK2C_SYNC) {
        return; _cilk_sync3:;
      }
      CILK2C_AFTER_SYNC_SLOW();
      CILK2C_AT_THREAD_BOUNDARY_SLOW();
    }
    {
      { int __tmp=(_cilk_frame->scope2.x+_cilk_frame->scope2.y);
        Cilk_set_result(_cilk_ws,&__tmp,sizeof((__tmp)));
      }
      CILK2C_BEFORE_RETURN_SLOW();
      return;
    }
  }
}
}
""" )

    | _ -> error ("non supported Cilk function: " ++ fname.name ++ "\n\n")
    end;


-- Import Function --------------------------------------------------
  newDecls <- [ importDecl ];
  local importBody :: Stmt = makeImportBody(newName, args);
  local importDecl :: Decl = makeImportFunction(newName, importBody);


-- Export Function --------------------------------------------------
  newDecls <- [ exportDecl ];
  local exportBody :: Stmt = makeExportBody(newName, bty, args);
  local exportDecl :: Decl = makeExportFunction(newName, bty, args, exportBody);
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

abstract production fastClone
d::Decl ::= bty::BaseTypeExpr mty::TypeModifierExpr  newName::Name
  dcls::Decls  body::Stmt
{
  local newParams::TypeModifierExpr =
    case mty of
    | functionTypeExprWithArgs(ret, args, variadic) -> 
      functionTypeExprWithArgs(
         ret, 
         consParameters( 
           parameterDecl([], 
             typedefTypeExpr( [], name("CilkWorkerState",location=loc("ToDo",-10,-1,-1,-1,-1,-1))), 
             pointerTypeExpr([constQualifier()],baseTypeExpr()),
             justName( name( "_cilk_ws", location=loc("ToDo",-11,-1,-1,-1,-1,-1))),
             []),
             args
         )
         ,
         variadic
       )

    | functionTypeExprWithoutArgs(ret, ids) ->
           error("Cilk ToDo:Function: why do we use functionTypeExprWithoutArgs?")

    | _ -> error("ToDo: fix this in Cilk ext.  Violating some rules about extensibility.")
    end;


  forwards to decls ( foldDecl ( [
    txtDecl("#undef CILK_WHERE_AM_I"),
    txtDecl("#define CILK_WHERE_AM_I IN_FAST_PROCEDURE"),

    -- The fast clone has the header
    --  `signed int fib(CilkWorkerState  *const  _cilk_ws, signed int  n)`
    functionDeclaration(
      functionDecl ( [], [], bty, newParams, newName, [], dcls, 
--         compoundStmt(
           foldStmt ( [
             txtStmt(s"struct _cilk_${newName.name}_frame*_cilk_frame;"),
             txtStmt(s"CILK2C_INIT_FRAME(_cilk_frame,sizeof(struct _cilk_${newName.name}_frame),_cilk_${newName.name}_sig);"),
             txtStmt(s"CILK2C_START_THREAD_FAST();"),
              body ] ) 
  -- )
      )
    )
    ] ) )
    with { env = addEnv ([ miscDef(cilk_in_fast_clone_id, emptyMiscItem()) ], d.env); } ;
}

abstract production slowClone
d::Decl ::= bty::BaseTypeExpr mty::TypeModifierExpr  newName::Name
  dcls::Decls  body::Stmt
{
  forwards to
    functionDeclaration(
      functionDecl ( [], [], bty, mty, newName, [], 
        dcls, -- ?
        body
      )
    )
    with { env = addEnv ([ miscDef(cilk_in_slow_clone_id, emptyMiscItem()) ], d.env); } ;
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

