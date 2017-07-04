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
top::ParameterDecl ::= storage::[StorageClass] bty::BaseTypeExpr mty::TypeModifierExpr name::MaybeName attrs::Attributes
{
  local n :: Name =
    case name.maybename of
    | just(n1) -> n1
    | _            -> error("cilk function parameter must be named")
    end;

  local scopeId :: String = toString(n.location.line);

  top.cilkFrameDeclsScope =
    pair(
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

