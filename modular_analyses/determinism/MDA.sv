grammar edu:umn:cs:melt:exts:ableC:cilk:modular_analyses:determinism;

import edu:umn:cs:melt:ableC:host only ablecParser;

copper_mda testCilkSpawn(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:spawnStmt;
}

copper_mda testFunction(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:functionDef;
}

copper_mda testCilkReturn(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:returnStmt;
}

copper_mda testCilkSync(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:syncStmt;
}

copper_mda testCilkExit(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:exitStmt;
}


