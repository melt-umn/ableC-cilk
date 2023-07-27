grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

-- struct declarators to be put in the cilk_functionname_frame structures
synthesized attribute cilkFrameDecls :: StructDeclarators occurs on Declarators;
synthesized attribute cilkFrameDecl  :: StructDeclarator  occurs on Declarator;

-- StructItemList to be put into scopes in cilk frame
synthesized attribute cilkFrameDeclsScope :: Pair<String StructItem> occurs on ParameterDecl;
synthesized attribute scopeId :: String occurs on Declarators, Declarator;

aspect production consDeclarator
top::Declarators ::= h::Declarator t::Declarators
{
  top.cilkFrameDecls = consStructDeclarator(h.cilkFrameDecl, t.cilkFrameDecls);
  -- declarators will all be in same scope so just use the first one
  top.scopeId = h.scopeId;
}

aspect production nilDeclarator
top::Declarators ::=
{
  top.cilkFrameDecls = nilStructDeclarator();
  top.scopeId = error("cannot get scopeId from nilDeclarator");
}

aspect production declarator
top::Declarator ::= name::Name ty::TypeModifierExpr attrs::Attributes
                    initializer::MaybeInitializer
{
  top.scopeId =
    if name.location.line >= 0
    then toString(name.location.line)
    else "gen";
  top.defs <- [scopeIdDef(name.name, top.scopeId)];

  top.cilkFrameDecl = structField(name, ty, attrs);
}

aspect production errorDeclarator
top::Declarator ::= msg::[Message]
{
  -- TODO: is this the right thing to do?
  top.cilkFrameDecl = warnStructField(msg);
  top.scopeId = error("cannot get scopeId from errorDeclarator");
}

aspect production consParameters
top::Parameters ::= h::ParameterDecl t::Parameters
{
  top.cilkFrameDeclsScopes =
    cons(h.cilkFrameDeclsScope, t.cilkFrameDeclsScopes);
}

aspect production nilParameters
top::Parameters ::=
{
  top.cilkFrameDeclsScopes = [];
}

aspect production parameterDecl
top::ParameterDecl ::= storage::StorageClasses bty::BaseTypeExpr mty::TypeModifierExpr name::MaybeName attrs::Attributes
{
  local n :: Name =
    case name.maybename of
    | just(n1) -> n1
    | _            -> error("cilk function parameter must be named")
    end;

  local scopeId :: String = toString(n.location.line);

  top.cilkFrameDeclsScope =
    (
      scopeId,
      structItem(
        attrs,
        bty,
        foldStructDeclarator([
          structField(n, mty, nilAttribute())
        ])
      )
    );

  top.defs <-
    case name.maybename of
    | just(_) ->
      [scopeIdDef(n.name, scopeId)]
    | _       -> []
    end;
}

aspect production consDecl
top::Decls ::= h::Decl  t::Decls
{
  top.cilkFrameDeclsScopes = h.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes;
}

aspect production nilDecl
top::Decls ::=
{
  top.cilkFrameDeclsScopes = [];
}

aspect production decls
top::Decl ::= d::Decls
{
  top.cilkFrameDeclsScopes = d.cilkFrameDeclsScopes;
}

aspect production defsDecl
top::Decl ::= d::[Def]
{
  top.cilkFrameDeclsScopes = [];
}

aspect production injectGlobalDeclsDecl
top::Decl ::= decls::Decls
{
  top.cilkFrameDeclsScopes = [];
}

aspect production injectFunctionDeclsDecl
top::Decl ::= decls::Decls
{
  top.cilkFrameDeclsScopes = [];
}

aspect production variableDecls
top::Decl ::= storage::StorageClasses  attrs::Attributes  ty::BaseTypeExpr  dcls::Declarators
{
  top.cilkFrameDeclsScopes = [(dcls.scopeId, structItem(attrs, ty, dcls.cilkFrameDecls))];
}

aspect production typeExprDecl
top::Decl ::= attrs::Attributes ty::BaseTypeExpr
{
  top.cilkFrameDeclsScopes = [];
}

aspect production typedefDecls
top::Decl ::= attrs::Attributes  ty::BaseTypeExpr  dcls::Declarators
{
  top.cilkFrameDeclsScopes = [];
}

aspect production functionDeclaration
top::Decl ::= f::FunctionDecl
{
  top.cilkFrameDeclsScopes = [];
}

aspect production warnDecl
top::Decl ::= msg::[Message]
{
  top.cilkFrameDeclsScopes = [];
}

aspect production decDecl
top::Decl ::= d::Decorated Decl
{
  top.cilkFrameDeclsScopes = d.cilkFrameDeclsScopes;
}

aspect production deferredDecl
top::Decl ::= refId::String  d::Decl
{
  -- TODO: Not sure if this is right, good enough for now?
  top.cilkFrameDeclsScopes = d.cilkFrameDeclsScopes;
}

aspect production staticAssertDecl
top::Decl ::= e::Expr  s::String
{
  top.cilkFrameDeclsScopes = [];
}

aspect production fileScopeAsm
top::Decl ::= s::String
{
  top.cilkFrameDeclsScopes = [];
}

aspect production autoDecl
top::Decl ::= name::Name  e::Expr
{
  local scopeId::String =
    if name.location.line >= 0
    then toString(name.location.line)
    else "gen";
  top.defs <- [scopeIdDef(name.name, scopeId)];

  top.cilkFrameDeclsScopes =
    [(scopeId,
      structItem(
        nilAttribute(),
        directTypeExpr(e.typerep),
        foldStructDeclarator([structField(name, baseTypeExpr(), nilAttribute())])))];
}

aspect production txtDecl
top::Decl ::= txt::String
{
  top.cilkFrameDeclsScopes = [];
}

