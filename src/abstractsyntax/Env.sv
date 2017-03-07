grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax;

synthesized attribute scopeIds :: Scope<String> occurs on Env;
synthesized attribute scopeIdContribs :: Contribs<String> occurs on Defs, Def;
--synthesized attribute scopeId :: String occurs on Env, Decl;

-- TODO: do we need envSyncLocations/Contribs? can envSyncLocationsDef be passed down through addEnv without them?
synthesized attribute envSyncLocations :: Scope<[Location]> occurs on Env;
synthesized attribute envSyncLocationsContribs :: Contribs<[Location]> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
--  top.scopeId = "0";
  top.scopeIds = [tm:empty(compareString)];
  top.envSyncLocations = [tm:empty(compareString)];
}

aspect production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
--  top.scopeId = e.scopeId;
  top.scopeIds = augmentScope_i(d.scopeIdContribs, e.scopeIds);
  top.envSyncLocations = augmentScope_i(d.envSyncLocationsContribs, e.envSyncLocations);
}

aspect production openScope_i
top::Env ::= e::Decorated Env
{
--  top.scopeId = toString(genInt());
  top.scopeIds = tm:empty(compareString) :: e.scopeIds;
  top.envSyncLocations = tm:empty(compareString) :: e.envSyncLocations;
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
  top.scopeIdContribs = [pair(s, scopeId)];
}

function lookupScopeId
[String] ::= n::String  e::Decorated Env
{
  return readScope_i(n, e.scopeIds);
}

abstract production syncLocationsDef
top::Def ::= s::String syncLocations::[Location]
{
  top.envSyncLocationsContribs = [pair(s, syncLocations)];
}

function lookupSyncLocations
[[Location]] ::= n::String  e::Decorated Env
{
  return readScope_i(n, e.envSyncLocations);
}

