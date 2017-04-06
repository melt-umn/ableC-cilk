grammar edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:cilk;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:concretesyntax:lexerHack as lh;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;


marking terminal Cilk_t 'cilk' lexer classes {Ckeyword};

concrete production cilk_func_c
top::Stmt_c ::= 'cilk' rhs::CilkRhs_c
{
  top.ast = rhs.ast;
}


closed nonterminal CilkRhs_c with location, ast<abs:Stmt>;
concrete productions top::CilkRhs_c
| f::CilkFunctionDefinition_c
  {
    top.ast = f.ast;
  }
| rb::ReturnBody
  {
    top.ast = rb.ast;
  }

closed nonterminal CilkFunctionDefinition_c with location, ast<abs:Stmt>;
concrete productions top::CilkFunctionDefinition_c
| d::CilkInitialFunctionDefinition_c  s::CompoundStatement_c
  {
    top.ast = abs:declStmt(d.ast);
    d.givenStmt = s.ast;
  }
  action {
    context = lh:closeScope(context); -- Opened by InitialFunctionDefinition.
  }
| ds::DeclarationSpecifiers_c d::Declarator_c ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    d.givenType = abs:baseTypeExpr();
    local bt :: abs:BaseTypeExpr =
      abs:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);

    top.ast =
      abs:declStmt(
        cilkFunctionProto(
          ds.storageClass, ds.specialSpecifiers, bt, d.ast,
          d.declaredIdent, ds.attributes
        )
      );
  }

closed nonterminal CilkInitialFunctionDefinition_c with location, ast<abs:Decl>, givenStmt;
concrete productions top::CilkInitialFunctionDefinition_c
| ds::DeclarationSpecifiers_c  d::Declarator_c  l::DeclarationList_c
    {
      ds.givenQualifiers = ds.typeQualifiers;
      d.givenType = abs:baseTypeExpr();

      local bt :: abs:BaseTypeExpr =
        abs:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);

      top.ast =
        cilkFunctionDecl(ds.storageClass, ds.specialSpecifiers, bt, d.ast, d.declaredIdent, ds.attributes, abs:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Function are annoying because we have to open a scope, then add the
      -- parameters, and close it after the brace.
      context = lh:beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }
| d::Declarator_c  l::DeclarationList_c
    {
      d.givenType = abs:baseTypeExpr();
      local bt :: abs:BaseTypeExpr =
        abs:figureOutTypeFromSpecifiers(d.location, [], [], [], []);

      top.ast =
        cilkFunctionDecl([], [], bt, d.ast, d.declaredIdent, [], abs:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Unfortunate duplication. This production is necessary for K&R compatibility
      -- We can't make it a proper optional nonterminal, since that requires a reduce far too early.
      -- (i.e. LALR conflicts)
      context = lh:beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }

nonterminal ReturnBody with location, ast<abs:Stmt> ;
concrete productions rb::ReturnBody
| 'return' ';'
    { rb.ast = cilk_returnStmt(abs:nothingExpr()); }
| 'return' rv::Expr_c ';'
    { rb.ast = cilk_returnStmt(abs:justExpr(rv.ast)); }

