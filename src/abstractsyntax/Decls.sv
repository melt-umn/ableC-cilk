grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

-- struct declarators to be put in the cilk_functionname_frame structures
synthesized attribute cilkFrameDecls :: StructDeclarators occurs on Declarators;
synthesized attribute cilkFrameDecl  :: StructDeclarator  occurs on Declarator;

-- name and scope of variable in cilk frame
synthesized attribute cilkFrameVar :: Pair<Name Integer> occurs on ParameterDecl;

-- StructItemList to be put into scopes in cilk frame
synthesized attribute cilkFrameDeclsScope :: StructItem occurs on ParameterDecl;

aspect production root
top::Root ::= d::Decls
{
  d.scopeCountInh = -1;
}

aspect production consDeclarator
top::Declarators ::= h::Declarator t::Declarators
{
  top.cilkFrameDecls = consStructDeclarator(h.cilkFrameDecl, t.cilkFrameDecls);
  top.cilkFrameVarsLocal = h.cilkFrameVarsLocal ++ t.cilkFrameVarsLocal;
}

aspect production nilDeclarator
top::Declarators ::=
{
  top.cilkFrameDecls = nilStructDeclarator();
  top.cilkFrameVarsLocal = [];
}

aspect production declarator
top::Declarator ::= name::Name ty::TypeModifierExpr attrs::[Attribute]
                    initializer::MaybeInitializer
{
  top.cilkFrameDecl = structField(name, ty, attrs);
  top.cilkFrameVarsLocal = [pair(name, top.scopeCountInh)];
}

aspect production errorDeclarator
top::Declarator ::= msg::[Message]
{
  -- TODO: is this the right thing to do?
  top.cilkFrameDecl = warnStructField(msg);
  top.cilkFrameVarsLocal = [];
}

aspect default production
top::Decl ::=
{
  top.cilkFrameVarsLocal = [];
}

aspect production variableDecls
top::Decl ::= storage::[StorageClass] attrs::[Attribute] ty::BaseTypeExpr
              dcls::Declarators
{
  top.cilkFrameVarsLocal = dcls.cilkFrameVarsLocal;
}

aspect production consParameters
top::Parameters ::= h::ParameterDecl t::Parameters
{
  top.cilkFrameDeclsScopes =
    cons(
      cons(h.cilkFrameDeclsScope, head(t.cilkFrameDeclsScopes)),
      tail(t.cilkFrameDeclsScopes)
    );
  top.cilkFrameVarsLocal = cons(h.cilkFrameVar, t.cilkFrameVarsLocal);
}

aspect production nilParameters
top::Parameters ::=
{
  top.cilkFrameDeclsScopes = [[]];
  top.cilkFrameVarsLocal = [];
}

aspect production parameterDecl
top::ParameterDecl ::= storage::[StorageClass] bty::BaseTypeExpr mty::TypeModifierExpr name::MaybeName attrs::[Attribute]
{
  local n :: Name =
    case name of
    | justName(n1) -> n1
    | _            -> error("cilk function parameter must be named")
    end;

  top.cilkFrameDeclsScope =
    structItem(
      attrs,
      bty,
      foldStructDeclarator([
        structField(n, mty, [])
      ])
    );

  top.cilkFrameVar = pair(n, 0);
}

