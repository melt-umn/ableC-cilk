grammar edu:umn:cs:melt:exts:ableC:cilk:src:abstractsyntax ;

abstract production cilkSpawnExpr
s::Expr ::= l::Expr op::AssignOp f::Expr args::Exprs
{
  s.pp = concat([ text("spawn"), space(), l.pp, space(), op.pp, space(), 
                  f.pp, parens( ppImplode(text(","),args.pps) ) ]);

  --s.errors := [] ; -- TODO .... l.type   ++ f.erros ++ args.errors ;
  forwards to 
    binaryOpExpr(
      l, assignOp(op, location=s.location),   
      case f of
      | declRefExpr(id) -> directCallExpr(id, args, location=s.location)
      | _ -> callExpr(f, args, location=s.location)
      end,
      location=s.location);
}




