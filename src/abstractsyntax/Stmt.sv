grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

autocopy attribute syncCountInh :: Integer occurs on Stmt;
synthesized attribute syncCount :: Integer occurs on Stmt;

autocopy attribute scopeCountInh :: Integer occurs on Stmt, Declarators, Declarator, Decls, Decl;
synthesized attribute scopeCount :: Integer occurs on Stmt;

aspect production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
  body.scopeCountInh = 0;
}

aspect production nestedFunctionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
  body.scopeCountInh = 0;
}

aspect default production
top::Stmt ::=
{
  top.syncCount = top.syncCountInh;
  top.scopeCount = top.scopeCountInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  t.syncCountInh = h.syncCount;
  top.syncCount = t.syncCount;

  t.scopeCountInh = h.scopeCount;
  top.scopeCount = t.scopeCount;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  s.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = s.scopeCount;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  e.syncCountInh = t.syncCount;
  top.syncCount = e.syncCount;
  t.scopeCountInh = top.scopeCountInh + 1;
  e.scopeCountInh = t.scopeCount + 1;
  top.scopeCount = e.scopeCount;
}

aspect production ifStmtNoElse
top::Stmt ::= c::Expr t::Stmt
{
  top.syncCount = t.syncCount;
  t.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = t.scopeCount;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
}

