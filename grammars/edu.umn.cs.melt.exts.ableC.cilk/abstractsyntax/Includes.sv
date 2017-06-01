grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax;

{-
aspect production root
top::Root ::= d::Decls
{
  cpp_directives <- ["#include <cilk.h>", "#include <cilk-cilk2c.h>", 
                     "#include <cilk-cilk2c-pre.h>" ];
}
-}
