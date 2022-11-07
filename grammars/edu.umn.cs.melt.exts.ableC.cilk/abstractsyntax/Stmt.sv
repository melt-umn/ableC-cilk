grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

-- keep track of how many times we sync in order to generate entry labels
synthesized attribute syncLocations :: [Location] occurs on Stmt;

-- StructItemList to be put into scopes in cilk frame
synthesized attribute cilkFrameDeclsScopes :: [Pair<String StructItem>] occurs on Stmt, Expr, Exprs, ExprOrTypeName, Parameters, Decls, Decl;

inherited   attribute cilkLinksInh :: [Init] occurs on Stmt;
synthesized attribute cilkLinks    :: [Init] occurs on Stmt;

inherited   attribute cilkProcName :: Name occurs on Stmt;
propagate cilkLinksInh, cilkProcName on Stmt excluding
   seqStmt, ifStmt;

aspect production nullStmt
top::Stmt ::=
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  top.syncLocations = h.syncLocations ++ t.syncLocations;

  top.cilkFrameDeclsScopes = h.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes;

  propagate cilkProcName;

  h.cilkLinksInh = top.cilkLinksInh;
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

aspect production warnStmt
top::Stmt ::= msg::[Message]
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production decStmt
top::Stmt ::= s::Decorated Stmt
{
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.syncLocations = s.syncLocations;
  top.cilkLinks =
    -- TODO: This is a slight performance issue since we are redecorating s,
    -- and is annoying to have to write.  Some notion of decorated forwarding
    -- to replace these dec* productions would be nice to have instead.
    decorate new(s) with {
      env = top.env;
      controlStmtContext = top.controlStmtContext;  
      cilkLinksInh = top.cilkLinksInh;
      cilkProcName = top.cilkProcName;
    }.cilkLinks;
}

aspect production declStmt
top::Stmt ::= d::Decl
{
  top.cilkFrameDeclsScopes = d.cilkFrameDeclsScopes;
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  top.syncLocations = t.syncLocations ++ e.syncLocations;

  top.cilkFrameDeclsScopes = c.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes ++ e.cilkFrameDeclsScopes;

  propagate cilkProcName;

  t.cilkLinksInh = top.cilkLinksInh;
  e.cilkLinksInh = t.cilkLinks;
  top.cilkLinks = e.cilkLinks;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes ++ b.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes = b.cilkFrameDeclsScopes ++ e.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncLocations = b.syncLocations;

  top.cilkFrameDeclsScopes =
    case i of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end ++
    case c of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end ++
    case s of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end ++
    b.cilkFrameDeclsScopes;

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
    | _ -> error("for loop decl not variableDecls")
    end ++
    case c of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end ++
    case s of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end ++
    b.cilkFrameDeclsScopes;

  top.cilkLinks = b.cilkLinks;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncLocations = b.syncLocations;
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes ++ b.cilkFrameDeclsScopes;
  top.cilkLinks = b.cilkLinks;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = top.cilkLinks;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = v.cilkFrameDeclsScopes ++ s.cilkFrameDeclsScopes;
  top.cilkLinks = top.cilkLinks;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = s.cilkFrameDeclsScopes;
  top.cilkLinks = top.cilkLinks;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncLocations = s.syncLocations;
  top.cilkFrameDeclsScopes = l.cilkFrameDeclsScopes ++ u.cilkFrameDeclsScopes ++ s.cilkFrameDeclsScopes;
  top.cilkLinks = top.cilkLinks;
}

aspect production exprStmt
top::Stmt ::= d::Expr
{
  top.cilkFrameDeclsScopes = d.cilkFrameDeclsScopes;
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production txtStmt
s::Stmt ::= txt::String
{
  s.cilkFrameDeclsScopes = [];
  s.syncLocations = [];
  s.cilkLinks = s.cilkLinksInh;
}

aspect production asmStmt
top::Stmt ::= asm::AsmStatement
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production functionDeclStmt
top::Stmt ::= d::FunctionDecl
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production breakStmt
top::Stmt ::=
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production continueStmt
top::Stmt ::=
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production gotoStmt
top::Stmt ::= l::Name
{
  top.cilkFrameDeclsScopes = [];
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production returnStmt
top::Stmt ::= e::MaybeExpr
{
  top.cilkFrameDeclsScopes = case e of justExpr(e1) -> e1.cilkFrameDeclsScopes | nothingExpr() -> [] end;
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}

aspect production injectGlobalDeclsStmt
top::Stmt ::= decls::Decls lifted::Stmt
{
  top.cilkFrameDeclsScopes = lifted.cilkFrameDeclsScopes;
  top.syncLocations = [];
  top.cilkLinks = top.cilkLinksInh;
}
