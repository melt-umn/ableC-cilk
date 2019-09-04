grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

abstract production cilkForStmt
top::Stmt ::= i::MaybeExpr  c::MaybeExpr  s::MaybeExpr  b::Stmt
{
  top.pp =
    ppConcat([text("cilk"), space(), text("for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])),
      line(), braces(nestlines(2, b.pp)) ]);

  forwards to forStmt(i, c, s, b);
}

abstract production cilkForDeclStmt
top::Stmt ::= i::Decl  c::MaybeExpr  s::MaybeExpr  b::Stmt
{
  top.pp =
    ppConcat([text("cilk"), space(), text("for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])),
      line(), braces(nestlines(2, b.pp)) ]);

  forwards to forDeclStmt(i, c, s, b);
}

function cilkForBody
Stmt ::= f::Expr args::Exprs
{
  return
    compoundStmt(
      foldStmt([
        declStmt(
          variableDecls(nilStorageClass(), nilAttribute(),
            directTypeExpr(builtinType(nilQualifier(), signedType(intType()))),
            foldDeclarator([
              declarator(
                name("_u", location=builtinLoc(MODULE_NAME)),
                baseTypeExpr(),
                nilAttribute(),
                nothingInitializer()
              )
            ])
          )
        ),
        cilkSpawnStmt(
          declRefExpr(name("_u", location=builtinLoc(MODULE_NAME)),
            location=builtinLoc(MODULE_NAME)),
          f, args)
      ]));
}

