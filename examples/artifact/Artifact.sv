grammar artifact;

{- This Silver specification does little more than list the desired
   extensions. -}

import edu:umn:cs:melt:ableC:drivers:compile;

construct ableC as
  edu:umn:cs:melt:ableC:concretesyntax
translator using
  edu:umn:cs:melt:exts:ableC:cilk;

