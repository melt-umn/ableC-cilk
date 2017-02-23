grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

-- keep track of how many times we sync in order to generate entry labels
autocopy    attribute syncCountInh :: Integer occurs on Stmt;
synthesized attribute syncCount    :: Integer occurs on Stmt;

-- number each scope, match with scope structs in cilk frame
autocopy    attribute scopeCountInh :: Integer occurs on Stmt, Decl, Decls, Declarators, Declarator;
synthesized attribute scopeCount    :: Integer occurs on Stmt;

-- StructItemList to be put into scopes in cilk frame
autocopy    attribute cilkFrameDeclsScopesInh :: [[StructItem]] occurs on Stmt;
synthesized attribute cilkFrameDeclsScopes    :: [[StructItem]] occurs on Stmt, Parameters;

-- a list of all names and scopes of variables in cilk frame
autocopy    attribute cilkFrameVarsGlobal :: [Pair<Name Integer>] occurs on Stmt;
synthesized attribute cilkFrameVarsLocal  :: [Pair<Name Integer>] occurs on Stmt, Decl, Declarators, Declarator, Parameters;

autocopy    attribute cilkLinksInh :: [Init] occurs on Stmt;
synthesized attribute cilkLinks    :: [Init] occurs on Stmt;

autocopy    attribute cilkProcName :: Name occurs on Stmt;

aspect production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]
                      bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name
                      attrs::[Attribute]  decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
  body.scopeCountInh = 0;
  body.cilkFrameVarsGlobal = body.cilkFrameVarsLocal;
}

aspect default production
top::Stmt ::=
{
  top.syncCount = top.syncCountInh;
  top.scopeCount = top.scopeCountInh;
  top.cilkFrameDeclsScopes = top.cilkFrameDeclsScopesInh;
  top.cilkFrameVarsLocal = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  t.syncCountInh = h.syncCount;
  top.syncCount = t.syncCount;

  t.scopeCountInh = h.scopeCount;
  top.scopeCount = t.scopeCount;

  t.cilkFrameDeclsScopesInh = h.cilkFrameDeclsScopes;
  top.cilkFrameDeclsScopes = t.cilkFrameDeclsScopes;

  top.cilkFrameVarsLocal = h.cilkFrameVarsLocal ++ t.cilkFrameVarsLocal;

  t.cilkLinksInh = h.cilkLinks;
  top.cilkLinks = t.cilkLinks;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  s.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  s.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  top.scopeCount = s.scopeCount;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = s.cilkFrameVarsLocal;
  top.cilkLinks = s.cilkLinks;
}

aspect production declStmt
top::Stmt ::= d::Decl
{
  top.cilkFrameDeclsScopes =
    -- TODO: use syn attribute on Decl
    case d of
    | variableDecls(_, attrs, ty, dcls) ->
        -- push new decls on head of inherited list of lists
        cons(
          cons(structItem(attrs, ty, dcls.cilkFrameDecls), head(top.cilkFrameDeclsScopesInh)),
          tail(top.cilkFrameDeclsScopesInh)
        )
    end;

--    -- TODO: use syn attribute on Decl
--    case d of
--    | variableDecls(_, attrs, ty, dcls) ->
--        consStructItem(
--          structItem(
--            [],
--            structTypeExpr(
--              [],
--              structDecl(
--                [],
--                nothingName(),
--                foldStructItem([
--                  structItem(attrs, ty, dcls.cilkFrameDecls)
--                ]),
--                location=builtIn()
--              )
--            ),
--            foldStructDeclarator([
--              structField(
--                name("scope" ++ toString(top.scopeCount), location=builtIn()),
--                baseTypeExpr(),
--                []
--              )
--            ])
--          ),
--          top.cilkFrameDeclsScopesInh
--        )
--    end;

  top.cilkFrameVarsLocal = d.cilkFrameVarsLocal;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  e.syncCountInh = t.syncCount;
  top.syncCount = e.syncCount;

  t.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  e.scopeCountInh =
    if null(head(t.cilkFrameDeclsScopes))
    then t.scopeCount
    else t.scopeCount + 1;
  top.scopeCount = e.scopeCount;

  t.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  e.cilkFrameDeclsScopesInh = cons([], t.cilkFrameDeclsScopes);
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;

  top.cilkFrameVarsLocal = t.cilkFrameVarsLocal ++ e.cilkFrameVarsLocal;

  e.cilkLinksInh = t.cilkLinks;
  top.cilkLinks = e.cilkLinks;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;

  b.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;

  b.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkFrameVarsLocal = b.cilkFrameVarsLocal;
  top.cilkLinks = b.cilkLinks;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncCount = b.syncCount;

  b.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;

  b.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkFrameVarsLocal = b.cilkFrameVarsLocal;
  top.cilkLinks = b.cilkLinks;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;

  b.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;

  b.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkFrameVarsLocal = b.cilkFrameVarsLocal;
  top.cilkLinks = b.cilkLinks;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;

  b.cilkFrameDeclsScopesInh = cons([], top.cilkFrameDeclsScopesInh);
  top.cilkFrameDeclsScopes =
    -- TODO: use syn attribute on Decl
    case i of
    | variableDecls(_, attrs, ty, dcls) ->
        -- push new decls on head of inherited list of lists
        cons(
          cons(structItem(attrs, ty, dcls.cilkFrameDecls), head(top.cilkFrameDeclsScopesInh)),
          tail(top.cilkFrameDeclsScopesInh)
        )
    end;
--    case i of
--    | variableDecls(_, attrs, ty, dcls) ->
--        consStructItem(
--          structItem(
--            [],
--            structTypeExpr(
--              [],
--              structDecl(
--                [],
--                nothingName(),
--                foldStructItem([
--                  structItem(attrs, ty, dcls.cilkFrameDecls)
--                ]),
--                location=builtIn()
--              )
--            ),
--            foldStructDeclarator([
--              structField(
--                name("scope" ++ toString(b.scopeCountInh), location=builtIn()),
--                baseTypeExpr(),
--                []
--              )
--            ])
--          ),
--          top.cilkFrameDeclsScopesInh
--        )
--    end;

  top.cilkFrameVarsLocal = i.cilkFrameVarsLocal ++ b.cilkFrameVarsLocal;
  top.cilkLinks = b.cilkLinks;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh =
    if null(head(top.cilkFrameDeclsScopesInh))
    then top.scopeCountInh
    else top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = b.cilkFrameVarsLocal;
  top.cilkLinks = b.cilkLinks;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = s.cilkFrameVarsLocal;
  top.cilkLinks = s.cilkLinks;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = s.cilkFrameVarsLocal;
  top.cilkLinks = s.cilkLinks;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = s.cilkFrameVarsLocal;
  top.cilkLinks = s.cilkLinks;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkFrameVarsLocal = s.cilkFrameVarsLocal;
  top.cilkLinks = s.cilkLinks;
}

