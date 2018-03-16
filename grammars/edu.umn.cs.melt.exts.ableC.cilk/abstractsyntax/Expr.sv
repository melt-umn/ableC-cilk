grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

aspect production parenExpr
top::Expr ::= e::Expr
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
  top.cilkFrameDeclsScopes = f.cilkFrameDeclsScopes ++ a.cilkFrameDeclsScopes;
}

aspect production memberExpr
top::Expr ::= lhs::Expr  deref::Boolean  rhs::Name
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes;
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

aspect production alignofExpr
top::Expr ::= e::ExprOrTypeName
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production sizeofExpr
top::Expr ::= e::ExprOrTypeName
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production imagExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production realExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production notExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production bitNegateExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production negativeExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production positiveExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production dereferenceExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production addressOfExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production postDecExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production postIncExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production preDecExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production preIncExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production eqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production mulEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production divEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production modEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production addEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production subEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production lshEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production rshEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production andEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production xorEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production orEqExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production andExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production orExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production andBitExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production orBitExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production xorExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production lshExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production rshExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production equalsExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production notEqualsExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production gtExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production ltExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production gteExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production lteExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production addExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production subExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production mulExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production divExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production modExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production commaExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  top.cilkFrameDeclsScopes = lhs.cilkFrameDeclsScopes ++ rhs.cilkFrameDeclsScopes;
}

aspect production typesCompatibleExpr
top::Expr ::= l::TypeName  r::TypeName
{
  top.cilkFrameDeclsScopes = [];
}

aspect production vaArgExpr
top::Expr ::= e::Expr  ty::TypeName
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production offsetofExpr
top::Expr ::= ty::TypeName  e::MemberDesignator
{
  top.cilkFrameDeclsScopes = [];
}

aspect production isConstantExpr
top::Expr ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production vaArgPackExpr
top::Expr ::= 
{
  top.cilkFrameDeclsScopes = [];
}

aspect production expectExpr
top::Expr ::= eval::Expr  expected::Expr
{
  top.cilkFrameDeclsScopes = eval.cilkFrameDeclsScopes ++ expected.cilkFrameDeclsScopes;
}

aspect production vaStartExpr
top::Expr ::= lastParam::Name  valist::Name
{
  top.cilkFrameDeclsScopes = [];
}

aspect production vaEndExpr
top::Expr ::= valist::Name
{
  top.cilkFrameDeclsScopes = [];
}

aspect production realConstant
top::Expr ::= c::NumericConstant
{
  top.cilkFrameDeclsScopes = [];
}

aspect production imaginaryConstant
top::Expr ::= c::NumericConstant
{
  top.cilkFrameDeclsScopes = [];
}

aspect production characterConstant
top::Expr ::= num::String  c::CharPrefix
{
  top.cilkFrameDeclsScopes = [];
}

aspect production declRefExpr
top::Expr ::= id::Name
{
  top.cilkFrameDeclsScopes = [];
}

aspect production stringLiteral
top::Expr ::= l::String
{
  top.cilkFrameDeclsScopes = [];
}

aspect production compoundLiteralExpr
top::Expr ::= ty::TypeName  positionalInit::InitList
{
  top.cilkFrameDeclsScopes = [];
}

aspect production errorExpr
top::Expr ::= msg::[Message]
{
  top.cilkFrameDeclsScopes = [];
}

aspect production qualifiedExpr
top::Expr ::= q::Qualifiers e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production comment
top::Expr ::= s::String
{
  top.cilkFrameDeclsScopes = [];
}

aspect production txtExpr
e::Expr ::= txt::String
{
  e.cilkFrameDeclsScopes = [];
}

aspect production predefinedFuncExpr
top::Expr ::= 
{
  top.cilkFrameDeclsScopes = [];
}

aspect production injectGlobalDeclsExpr
top::Expr ::= decls::Decls lifted::Expr
{
  top.cilkFrameDeclsScopes = lifted.cilkFrameDeclsScopes;
}

aspect production consExpr
top::Exprs ::= h::Expr  t::Exprs
{
  top.cilkFrameDeclsScopes = h.cilkFrameDeclsScopes ++ t.cilkFrameDeclsScopes;
}

aspect production nilExpr
top::Exprs ::=
{
  top.cilkFrameDeclsScopes = [];
}

aspect production exprExpr
top::ExprOrTypeName ::= e::Expr
{
  top.cilkFrameDeclsScopes = e.cilkFrameDeclsScopes;
}

aspect production typeNameExpr
top::ExprOrTypeName ::= ty::TypeName
{
  top.cilkFrameDeclsScopes = [];
}

