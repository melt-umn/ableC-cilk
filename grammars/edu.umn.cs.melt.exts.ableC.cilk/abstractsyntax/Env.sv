grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

synthesized attribute scopeIds :: Scopes<String> occurs on Env;
synthesized attribute scopeIdContribs :: Contribs<String> occurs on Defs, Def;

-- TODO: do we need envSyncLocations/Contribs? can envSyncLocationsDef be passed down through addEnv without them?
synthesized attribute envSyncLocations :: Scopes<[Location]> occurs on Env;
synthesized attribute envSyncLocationsContribs :: Contribs<[Location]> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
  top.scopeIds = [tm:empty()];
  top.envSyncLocations = [tm:empty()];
}

aspect production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
  top.scopeIds = addScope(d.scopeIdContribs, e.scopeIds);
  top.envSyncLocations = addScope(d.envSyncLocationsContribs, e.envSyncLocations);
}

aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.scopeIds = tm:empty() :: e.scopeIds;
  top.envSyncLocations = tm:empty() :: e.envSyncLocations;
}

aspect production globalEnv_i
top::Env ::= e::Decorated Env
{
  top.scopeIds = [last(e.scopeIds)];
  top.envSyncLocations = [last(e.envSyncLocations)];
}

aspect production nonGlobalEnv_i
top::Env ::= e::Decorated Env
{
  top.scopeIds = init(e.scopeIds);
  top.envSyncLocations = init(e.envSyncLocations);
}

aspect production functionEnv_i
top::Env ::= e::Decorated Env
{
  top.scopeIds = functionScope(e.scopeIds);
  top.envSyncLocations = functionScope(e.envSyncLocations);
}

aspect production nilDefs
top::Defs ::=
{
  top.scopeIdContribs = [];
  top.envSyncLocationsContribs = [];
}

aspect production consDefs
top::Defs ::= h::Def  t::Defs
{
  top.scopeIdContribs = h.scopeIdContribs ++ t.scopeIdContribs;
  top.envSyncLocationsContribs = h.envSyncLocationsContribs ++
    t.envSyncLocationsContribs;
}

aspect default production
top::Def ::=
{
  top.scopeIdContribs = [];
  top.envSyncLocationsContribs = [];
}

abstract production scopeIdDef
top::Def ::= s::String scopeId::String
{
  top.scopeIdContribs = [(s, scopeId)];
}

function lookupScopeId
[String] ::= n::String  e::Decorated Env
{
  return lookupScope(n, e.scopeIds);
}

abstract production syncLocationsDef
top::Def ::= s::String syncLocations::[Location]
{
  top.envSyncLocationsContribs = [(s, syncLocations)];
}

function lookupSyncLocations
[[Location]] ::= n::String  e::Decorated Env
{
  return lookupScope(n, e.envSyncLocations);
}

