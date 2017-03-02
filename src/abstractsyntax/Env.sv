grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

synthesized attribute scopeIds :: Scope<String> occurs on Env;
synthesized attribute scopeIdContribs :: Contribs<String> occurs on Defs, Def;
--synthesized attribute scopeId :: String occurs on Env, Decl;

aspect production emptyEnv_i
top::Env ::=
{
--  top.scopeId = "0";
  top.scopeIds = [tm:empty(compareString)];
}

aspect production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
--  top.scopeId = e.scopeId;
  top.scopeIds = augmentScope_i(d.scopeIdContribs, e.scopeIds);
}

aspect production openScope_i
top::Env ::= e::Decorated Env
{
--  top.scopeId = toString(genInt());
  top.scopeIds = tm:empty(compareString) :: e.scopeIds;
}

aspect production nilDefs
top::Defs ::=
{
  top.scopeIdContribs = [];
}

aspect production consDefs
top::Defs ::= h::Def  t::Defs
{
  top.scopeIdContribs = h.scopeIdContribs ++ t.scopeIdContribs;
}

aspect default production
top::Def ::=
{
  top.scopeIdContribs = [];
}

abstract production scopeIdDef
top::Def ::= s::String scopeId::String
{
  top.scopeIdContribs = [pair(s, scopeId)];
}

function lookupScopeId
[String] ::= n::String  e::Decorated Env
{
  return readScope_i(n, e.scopeIds);
}

