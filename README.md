TQL (Traversable data structure Query Language)
===

Combinatorial Query/Transformation language for Traversable data structures in Scala and in particular [scala.meta](http://scalameta.org) Trees.

The library comprise a set of combinators that you can compose in order to traverse or transform your data structures.

### Examples
Get all different variable names in a scala.meta AST
```scala
q"""
 var a = 5
 val c = 3
 if (3 == 17) {
  val c = 1
 }
 else 2
"""
val getAllVals = down(collectIn[Set]{case x: Defn.Val => x.pats.head.toString})
println(getAllVals(x).result) //Set(c)
```

### How to use
A snapshot is available on _Sonatype_. To use it with SBT, add the following lines in your build:
```scala
libraryDependencies += "com.github.begeric" % "tqlscalameta_2.11" % "0.1-SNAPSHOT"
resolvers += Resolver.sonatypeRepo("snapshots")
```
