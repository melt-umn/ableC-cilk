grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilk_syncStmt
s::Stmt ::= 
{
  s.pp = text("sync");

  forwards to nullStmt();
}

