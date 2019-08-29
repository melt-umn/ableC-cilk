grammar edu:umn:cs:melt:exts:ableC:cilk:abstractsyntax; 

imports silver:langutil;
imports silver:langutil:pp with implode as ppImplode, concat as ppConcat;

import edu:umn:cs:melt:ableC:parallel;

global MODULE_NAME :: String = "edu:umn:cs:melt:exts:ableC:cilk";
global PARALLEL_EXTENSION_NUMBER :: Integer = 
  head((decorate parallelism(emptyEnv()) with {extensionName=MODULE_NAME;}).extensionNumber);
