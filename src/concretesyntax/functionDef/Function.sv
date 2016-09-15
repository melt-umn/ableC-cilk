grammar edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:functionDef ;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:concretesyntax:lexerHack as lh;

imports edu:umn:cs:melt:ableC:abstractsyntax as ast;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as ast;


-- Some library utilities and the Cilk abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

marking terminal Cilk_t 'cilk' lexer classes {Ckeyword};

concrete production cilk_func_c
top::Declaration_c ::= 'cilk' 
  d::CilkInitialFunctionDefinition_c  s::CompoundStatement_c 
{ top.ast = d.ast;
  d.givenStmt = s.ast;
}
action {
  context = lh:closeScope(context); -- Opened by InitialFunctionDefinition.
}

closed nonterminal CilkInitialFunctionDefinition_c with location, ast<ast:Decl>, givenStmt;
concrete productions top::CilkInitialFunctionDefinition_c
| ds::DeclarationSpecifiers_c  d::Declarator_c  l::DeclarationList_c
    {
      ds.givenQualifiers = ds.typeQualifiers;
      d.givenType = ast:baseTypeExpr();
      
      local bt :: ast:BaseTypeExpr =
        ast:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
      
      top.ast = 
        cilkFunctionDecl(ds.storageClass, ds.specialSpecifiers, bt, d.ast, d.declaredIdent, ds.attributes, ast:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Function are annoying because we have to open a scope, then add the
      -- parameters, and close it after the brace.
      context = lh:beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }
| d::Declarator_c  l::DeclarationList_c
    {
      d.givenType = ast:baseTypeExpr();
      local bt :: ast:BaseTypeExpr =
        ast:figureOutTypeFromSpecifiers(d.location, [], [], [], []);

      top.ast = 
        cilkFunctionDecl([], [], bt, d.ast, d.declaredIdent, [], ast:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Unfortunate duplication. This production is necessary for K&R compatibility
      -- We can't make it a proper optional nonterminal, since that requires a reduce far too early.
      -- (i.e. LALR conflicts)
      context = lh:beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }


