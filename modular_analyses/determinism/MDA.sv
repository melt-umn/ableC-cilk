grammar determinism;

import edu:umn:cs:melt:ableC:host only ablecParser;

copper_mda testCilkSpawn(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:spawnStmt;
}

copper_mda testCilkFunction(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:functionDef;
}

copper_mda testCilkReturn(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:returnStmt;
}

copper_mda testCilkSync(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:syncStmt;
}

copper_mda testCilkExit(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:concretesyntax:exitStmt;
}


