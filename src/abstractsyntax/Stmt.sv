grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

autocopy attribute syncCountInh :: Integer occurs on Stmt;
synthesized attribute syncCount :: Integer occurs on Stmt;

autocopy attribute scopeCountInh :: Integer occurs on Stmt, Decl, Decls, Declarators, Declarator;
synthesized attribute scopeCount :: Integer occurs on Stmt;

autocopy attribute scopesInh :: StructItemList occurs on Stmt;
synthesized attribute scopes :: StructItemList occurs on Stmt;

aspect production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::Name  attrs::[Attribute]  decls::Decls  body::Stmt
{
  body.syncCountInh = 0;
  body.scopeCountInh = 0;
}

aspect default production
top::Stmt ::=
{
  top.syncCount = top.syncCountInh;
  top.scopeCount = top.scopeCountInh;
  top.scopes = top.scopesInh;
}

aspect production seqStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  t.syncCountInh = h.syncCount;
  top.syncCount = t.syncCount;

  t.scopeCountInh = h.scopeCount;
  top.scopeCount = t.scopeCount;

  t.scopesInh = h.scopes;
  top.scopes = t.scopes;
}

aspect production compoundStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  s.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = s.scopeCount;
  top.scopes = s.scopes;
}

aspect production declStmt
top::Stmt ::= d::Decl
{
  top.scopes =
    case d of
    | variableDecls(_, attrs, ty, dcls) ->
        consStructItem(
          structItem(
            [],
            structTypeExpr(
              [],
              structDecl(
                [],
                nothingName(),
                foldStructItem([
                  structItem(attrs, ty, makeStructDecls(dcls))
                ]),
                location=builtIn()
              )
            ),
            foldStructDeclarator([
              structField(
                name("scope" ++ toString(top.scopeCount), location=builtIn()),
                baseTypeExpr(),
                []
              )
            ])
          ),
          top.scopesInh
        )
    end;
}

aspect production ifStmt
top::Stmt ::= c::Expr t::Stmt e::Stmt
{
  e.syncCountInh = t.syncCount;
  top.syncCount = e.syncCount;
  t.scopeCountInh = top.scopeCountInh + 1;
  e.scopeCountInh = t.scopeCount + 1;
  top.scopeCount = e.scopeCount;
  e.scopesInh = t.scopes;
  top.scopes = e.scopes;
}

aspect production ifStmtNoElse
top::Stmt ::= c::Expr t::Stmt
{
  top.syncCount = t.syncCount;
  t.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = t.scopeCount;
  top.scopes = t.scopes;
}

aspect production whileStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
  top.scopes = b.scopes;
}

aspect production doStmt
top::Stmt ::= b::Stmt e::Expr
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
  top.scopes = b.scopes;
}

aspect production forStmt
top::Stmt ::= i::MaybeExpr c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
  top.scopes = b.scopes;
}

aspect production forDeclStmt
top::Stmt ::= i::Decl c::MaybeExpr s::MaybeExpr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;

  top.scopes =
    case i of
    | variableDecls(_, attrs, ty, dcls) ->
        consStructItem(
          structItem(
            [],
            structTypeExpr(
              [],
              structDecl(
                [],
                nothingName(),
                foldStructItem([
                  structItem(attrs, ty, makeStructDecls(dcls))
                ]),
                location=builtIn()
              )
            ),
            foldStructDeclarator([
              structField(
                name("scope" ++ toString(b.scopeCountInh), location=builtIn()),
                baseTypeExpr(),
                []
              )
            ])
          ),
          top.scopesInh
        )
    end;
}

aspect production switchStmt
top::Stmt ::= e::Expr b::Stmt
{
  top.syncCount = b.syncCount;
  b.scopeCountInh = top.scopeCountInh + 1;
  top.scopeCount = b.scopeCount;
  top.scopes = b.scopes;
}

aspect production labelStmt
top::Stmt ::= l::Name s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.scopes = s.scopes;
}

aspect production caseLabelStmt
top::Stmt ::= v::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.scopes = s.scopes;
}

aspect production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.scopes = s.scopes;
}

aspect production caseLabelRangeStmt
top::Stmt ::= l::Expr u::Expr s::Stmt
{
  top.syncCount = s.syncCount;
  top.scopeCount = s.scopeCount;
  top.scopes = s.scopes;
}

aspect production root
top::Root ::= d::Decls
{
  d.scopeCountInh = -1;
}

function makeStructDecls
StructDeclarators ::= dcls::Declarators
{
  return
    case dcls of
    | consDeclarator(h, t) ->
        case h of
        | declarator(n, ty, attrs, _) ->
            consStructDeclarator(
              structField(n, ty, attrs),
              makeStructDecls(t)
            )
        | errorDeclarator(msg) ->
            -- TODO: improve this message
            error("errorDeclarator found")
        end
    | nilDeclarator()      ->
        nilStructDeclarator()
    end;
}

