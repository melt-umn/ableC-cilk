grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

aspect default production
top::Expr ::=
{
  top.cilkFrameDeclsScopes = [];
}

aspect production parenExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production qualifiedUnaryOpExpr
top::Expr ::= op::UnaryOp   e::Expr  collectedTypeQualifiers::Qualifiers
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production arraySubscriptExpr
top::Expr ::= lhs::Expr  rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production callExpr
top::Expr ::= f::Expr  a::Exprs
{
  -- TODO: cilkFrameDeclsScopes on Exprs
  top.cilkFrameDeclsScopes = f.cilkFrameDeclsScopes;
}

aspect production memberExpr
top::Expr ::= lhs::Expr  deref::Boolean  rhs::Name
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes;
}

aspect production qualifiedBinaryOpExpr
top::Expr ::= lhs::Expr  op::BinOp  rhs::Expr  collectedTypeQualifiers::Qualifiers
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production conditionalExpr
top::Expr ::= cond::Expr  t::Expr  e::Expr
{
  top.cilkFrameDeclsScopes = cond.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes ++
    e.cilkFrameDeclsScopes;
}

aspect production binaryConditionalExpr
top::Expr ::= cond::Expr  e::Expr
{
  top.cilkFrameDeclsScopes = cond.cilkFrameDeclsScopes ++ e.cilkFrameDeclsScopes;
}

aspect production explicitCastExpr
top::Expr ::= ty::TypeName  e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production genericSelectionExpr
top::Expr ::= e::Expr  gl::GenericAssocs  def::MaybeExpr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes ++
    case def of justExpr(e) -> e.cilkFrameDeclsScopes | nothingExpr() -> [] end;
}

aspect production stmtExpr
top::Expr ::= body::Stmt result::Expr
{
  top.cilkFrameDeclsScopes = body.cilkFrameDeclsScopes ++ result.cilkFrameDeclsScopes;
}

