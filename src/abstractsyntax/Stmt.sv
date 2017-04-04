grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

-- keep track of how many times we sync in order to generate entry labels
synthesized attribute syncLocations :: [Location] occurs on Stmt;

-- StructItemList to be put into scopes in cilk frame
synthesized attribute cilkFrameDeclsScopes    :: [Pair<String StructItem>] occurs on Stmt, Parameters;

autocopy    attribute cilkLinksInh :: [Init] occurs on Stmt;
synthesized attribute cilkLinks    :: [Init] occurs on Stmt;

autocopy    attribute cilkProcName :: Name occurs on Stmt;

aspect default production
top::Stmt ::=
{
  top.syncLocations = [];
  top.cilkFrameDeclsScopes = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  top.syncLocations = h.syncLocations ++ t.syncLocations;

  top.cilkFrameDeclsScopes = h.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes;

  t.cilkLinksInh = h.cilkLinks;
  top.cilkLinks = t.cilkLinks;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = s.cilkLinks;
}

aspect production declStmt
top::Stmt ::= d::Decl
{
  top.cilkFrameDeclsScopes =
    case d of
    | variableDecls(_, attrs, ty, dcls) ->
        [pair(dcls.scopeId, structItem(attrs, ty, dcls.cilkFrameDecls))]
    end;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  top.syncLocations = t.syncLocations ++ e.syncLocations;

  top.cilkFrameDeclsScopes = t.cilkFrameDeclsScopes ++ e.cilkFrameDeclsScopes;

  e.cilkLinksInh = t.cilkLinks;
  top.cilkLinks = e.cilkLinks;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncLocations = b.syncLocations;
  top.cilkFrameDeclsScopes =
    case i of
    | variableDecls(_, attrs, ty, dcls) ->
        [pair(dcls.scopeId, structItem(attrs, ty, dcls.cilkFrameDecls))]
    end;

  top.cilkLinks = b.cilkLinks;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncLocations = b.syncLocations;
  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes;
  top.cilkLinks = b.cilkLinks;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = s.cilkLinks;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = s.cilkLinks;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = s.cilkLinks;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = s.cilkLinks;
}

aspect production returnStmt
top::Stmt ::= e::MaybeExpr
{
  local slow::Boolean = !null(lookupMisc(cilk_in_slow_clone_id, top.env));
  top.errors <-
    if   slow
    then [err(bogusLoc(), "cannot use standard return in Cilk function, use cilk_return instead")]
    else [];
}

