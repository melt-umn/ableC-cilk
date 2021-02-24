grammar edu:umn:cs:melt:exts:ableC:cilk:concretesyntax;

-- Import host language components
import edu:umn:cs:melt:ableC:concretesyntax;
import edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
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
    context = closeScope(context); -- Opened by InitialFunctionDefinition.
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
        abs:foldStorageClass(ds.storageClass), specialSpecifiers, bt, d.ast,
        d.declaredIdent, ds.attributes
      );
  }

closed nonterminal CilkInitialFunctionDefinition_c with location, ast<abs:Decl>, givenStmt;
concrete productions top::CilkInitialFunctionDefinition_c
| ds::DeclarationSpecifiers_c  d::Declarator_c  l::InitiallyUnqualifiedDeclarationList_c
    {
      ds.givenQualifiers = ds.typeQualifiers;
      d.givenType = abs:baseTypeExpr();
      l.givenQualifiers =
        case baseMT of
        | abs:functionTypeExprWithArgs(t, p, v, q) -> q
        | abs:functionTypeExprWithoutArgs(t, v, q) -> q
        | _ -> abs:nilQualifier()
        end;

      local specialSpecifiers :: abs:SpecialSpecifiers =
        foldr(abs:consSpecialSpecifier, abs:nilSpecialSpecifier(), ds.specialSpecifiers);
      
      local bt :: abs:BaseTypeExpr =
        abs:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
      
      -- If this is a K&R-style declaration, attatch any function qualifiers to the first declaration instead
      local baseMT  :: abs:TypeModifierExpr = d.ast;
      baseMT.abs:baseType = abs:errorType();
      baseMT.abs:typeModifierIn = abs:baseTypeExpr();
      baseMT.abs:returnType = nothing();
      baseMT.abs:breakValid = false;
      baseMT.abs:continueValid = false;
      local mt :: abs:TypeModifierExpr =
        case l.isDeclListEmpty, baseMT of
        | false, abs:functionTypeExprWithArgs(t, p, v, q) ->
          abs:functionTypeExprWithArgs(t, p, v, abs:nilQualifier())
        | false, abs:functionTypeExprWithoutArgs(t, v, q) ->
          abs:functionTypeExprWithoutArgs(t, v, abs:nilQualifier())
        | _, mt -> mt
        end;

      top.ast =
        cilkFunctionDecl(abs:foldStorageClass(ds.storageClass), specialSpecifiers, bt, mt, d.declaredIdent, ds.attributes, abs:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Function are annoying because we have to open a scope, then add the
      -- parameters, and close it after the brace.
      context = beginFunctionScope(d.declaredIdent, Identifier_t, d.declaredParamIdents, Identifier_t, context);
    }
| d::Declarator_c  l::InitiallyUnqualifiedDeclarationList_c
    {
      d.givenType = abs:baseTypeExpr();
      l.givenQualifiers =
        case baseMT of
        | abs:functionTypeExprWithArgs(t, p, v, q) -> q
        | abs:functionTypeExprWithoutArgs(t, v, q) -> q
        | _ -> abs:nilQualifier()
        end;
      
      local bt :: abs:BaseTypeExpr =
        abs:figureOutTypeFromSpecifiers(d.location, abs:nilQualifier(), [], [], []);
      
      -- If this is a K&R-style declaration, attatch any function qualifiers to the first declaration instead
      local baseMT  :: abs:TypeModifierExpr = d.ast;
      baseMT.abs:baseType = abs:errorType();
      baseMT.abs:typeModifierIn = abs:baseTypeExpr();
      baseMT.abs:returnType = nothing();
      baseMT.abs:breakValid = false;
      baseMT.abs:continueValid = false;
      local mt :: abs:TypeModifierExpr =
        case l.isDeclListEmpty, baseMT of
        | false, abs:functionTypeExprWithArgs(t, p, v, q) ->
          abs:functionTypeExprWithArgs(t, p, v, abs:nilQualifier())
        | false, abs:functionTypeExprWithoutArgs(t, v, q) ->
          abs:functionTypeExprWithoutArgs(t, v, abs:nilQualifier())
        | _, mt -> mt
        end;

      top.ast =
        cilkFunctionDecl(abs:nilStorageClass(), abs:nilSpecialSpecifier(), bt, mt, d.declaredIdent, abs:nilAttribute(), abs:foldDecl(l.ast), top.givenStmt);
    }
    action {
      -- Unfortunate duplication. This production is necessary for K&R compatibility
      -- We can't make it a proper optional nonterminal, since that requires a reduce far too early.
      -- (i.e. LALR conflicts)
      context = beginFunctionScope(d.declaredIdent, Identifier_t, d.declaredParamIdents, Identifier_t, context);
    }

