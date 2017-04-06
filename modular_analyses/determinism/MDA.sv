grammar edu:umn:cs:melt:exts:ableC:cilk:modular_analyses:determinism;

import edu:umn:cs:melt:ableC:host only ablecParser;

copper_mda testCilkSpawn(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:spawnStmt;
}

copper_mda testCilk(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:cilk;
}

copper_mda testCilkSync(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:syncStmt;
}

copper_mda testCilkExit(ablecParser) {
  edu:umn:cs:melt:exts:ableC:cilk:src:concretesyntax:exitStmt;
}


