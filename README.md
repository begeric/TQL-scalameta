TQL (Traversal Query Language)
===

Combinatorial Query/Transformation language for Traversable data structures in Scala and in particular [scala.meta](http://scalameta.org) Trees.

The library comprises a set of combinators that you can compose in order to traverse or transform your data structures.
For more information about its inner workings or an exhaustive list of combinators, please take a look at the [Wiki](https://github.com/begeric/TQL-scalameta/wiki).

### Examples
Get all different variable names in a scala.meta AST
```scala
q"""
 val a = 5
 val c = 3
 if (3 == 17) {
  val c = 1
 }
 else 2
"""
val getAllVals = down(collectIn[Set]{case x: Defn.Val => x.pats.head.toString}).result
println(getAllVals(x)) //Set(a, c)
```

When we know that we will only use the traversal once for a specific tree, we can use this more conveniant way of writing traversals:
```scala
val getAllVals = x.collectIn[Set]{case x: Defn.Val => x.pats.head.toString}.result //down is implicit
println(getAllVals)
```
Of course we lose the combining power of combinators. 

### How to use
A snapshot is available on _Sonatype_. To use it with SBT, add the following lines in your build:
```scala
libraryDependencies += "com.github.begeric" % "tqlscalameta_2.11" % "0.1-SNAPSHOT"
resolvers += Resolver.sonatypeRepo("snapshots")
```
