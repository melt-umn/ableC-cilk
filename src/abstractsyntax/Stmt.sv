grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

autocopy attribute syncCountInh :: Integer occurs on Stmt;
synthesized attribute syncCount :: Integer occurs on Stmt;

aspect production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
}

aspect production nestedFunctionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute] decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
}

aspect default production
top::Stmt ::=
{
  top.syncCount = top.syncCountInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  t.syncCountInh = h.syncCount;
  top.syncCount = t.syncCount;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  e.syncCountInh = t.syncCount;
  top.syncCount = e.syncCount;
}

aspect production ifStmtNoElse
top::Stmt ::= c::Expr t::Stmt
{
  top.syncCount = t.syncCount;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncCount = b.syncCount;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncCount = s.syncCount;
}

