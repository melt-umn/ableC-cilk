grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:concretesyntax:lexerHack as lh;
import edu:umn:cs:melt:ableC:abstractsyntax as abs;
import edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;

-- Some library utilities and the Cilk abstract syntax
import silver:langutil;
import edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

concrete production cilk_func_c
top::Declaration_c ::= 'cilk' f::CilkFunctionDefinition_c
{
  top.ast = f.ast;
}

closed nonterminal CilkFunctionDefinition_c with location, ast<abs:Decl>;
concrete productions top::CilkFunctionDefinition_c
| d::CilkInitialFunctionDefinition_c  s::CompoundStatement_c
  {
    top.ast = d.ast;
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

    local specialSpecifiers :: abs:SpecialSpecifiers =
      foldr(abs:consSpecialSpecifier, abs:nilSpecialSpecifier(), ds.specialSpecifiers);

    top.ast =
      cilkFunctionProto(
        ds.storageClass, specialSpecifiers, bt, d.ast,
        d.declaredIdent, ds.attributes
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

      local specialSpecifiers :: abs:SpecialSpecifiers =
        foldr(abs:consSpecialSpecifier, abs:nilSpecialSpecifier(), ds.specialSpecifiers);

      top.ast =
        cilkFunctionDecl(ds.storageClass, specialSpecifiers, bt, d.ast, d.declaredIdent, ds.attributes, abs:foldDecl(l.ast), top.givenStmt);
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
        abs:figureOutTypeFromSpecifiers(d.location, abs:nilQualifier(), [], [], []);

      top.ast =
        cilkFunctionDecl([], abs:nilSpecialSpecifier(), bt, d.ast, d.declaredIdent, abs:nilAttribute(), abs:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Unfortunate duplication. This production is necessary for K&R compatibility
      -- We can't make it a proper optional nonterminal, since that requires a reduce far too early.
      -- (i.e. LALR conflicts)
      context = lh:beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }

