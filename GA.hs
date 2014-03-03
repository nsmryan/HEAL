module GA(
  galf,
  garecomb
)
where

import EA
import Recombine
import Selection
import Randomly

garecomb pm pc pop = mutation pm pop >>= crossover 1 pc

galf ps is pm pc gens eval =
  ga (rndPopFrom ps is True)
     (evaluate eval)
     tournament
     (garecomb pm pc)
     False
     (maxGens gens)
