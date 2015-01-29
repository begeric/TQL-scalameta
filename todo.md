# TODO

### Things to implement/try out

* Stateful traversal
  * Manage global and path states
  * What is the link between stateful traversal and inherited parameters in attribute grammars?
* Lazy reconstruction (and have 'one pass' traversals ? see repmin)
* More profiling/benchmarks/optimizations for the scala.meta traverser 
* Remove composition overhead (via macros)
* Separate traversal and transformation at the implementation level (easier when combined with the above point)
* Make MatchResult be any Monad?
  * Can then use Future (to have paralelization)
  * Have the State Monad to make stateful traversal
* Play with parallelization instead of Fusing
* Implement Fusing for more traversal strategies
* Generalize one-pass traversals (see until and tupledUntil combinators/examples)
* Improve the XPath-like API
* Implement symbol lookup
* more generic traversal strategies (top-down/bottom-up, dfs/bfs, break, continue, until)
* more specific return type for combinators: U <: T, x: U => combinator(x): U (and not T as it is currently)
