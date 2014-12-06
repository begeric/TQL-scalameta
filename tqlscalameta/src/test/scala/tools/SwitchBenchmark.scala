package tools

/**
 * Created by Eric on 03.12.2014.
 */
import org.scalameter._
import org.scalameter.execution.LocalExecutor
import org.scalameter.reporting.LoggingReporter

object SwitchBenchmark  extends PerformanceTest {


  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val range = Gen.range("vals")(0,10,1)
  val nbIters = 100000

  val fun = (s: Int, res: Int) => s + res

  val table = new Array[(Int, Int) => Int](50)
  for (i <- 0 until 50)
    table(i) = fun

  performance of "Access" in {
    measure method "Array" in {
      using(range) in { j =>
        var res1 = 0
        var i = 0
        while (i < nbIters) {
          res1 = table(i % 50)(1, res1)
          i += 1
        }
      }
    }
  }

  performance of "Access" in {
    measure method "pattern match" in {
      using(range) in { j =>
        var res2 = 0
        var i = 0
        while (i < nbIters){
          (i % 50) match {
            case 0 => res2 = 1 + res2
            case 1 => res2 = 1 + res2
            case 2 => res2 = 1 + res2
            case 3 => res2 = 1 + res2
            case 4 => res2 = 1 + res2
            case 5 => res2 = 1 + res2
            case 6 => res2 = 1 + res2
            case 7 => res2 = 1 + res2
            case 8 => res2 = 1 + res2
            case 9 => res2 = 1 + res2
            case 10 => res2 = 1 + res2
            case 11 => res2 = 1 + res2
            case 12 => res2 = 1 + res2
            case 13 => res2 = 1 + res2
            case 14 => res2 = 1 + res2
            case 15 => res2 = 1 + res2
            case 16 => res2 = 1 + res2
            case 17 => res2 = 1 + res2
            case 18 => res2 = 1 + res2
            case 19 => res2 = 1 + res2
            case 20 => res2 = 1 + res2
            case 21 => res2 = 1 + res2
            case 22 => res2 = 1 + res2
            case 23 => res2 = 1 + res2
            case 24 => res2 = 1 + res2
            case 25 => res2 = 1 + res2
            case 26 => res2 = 1 + res2
            case 27 => res2 = 1 + res2
            case 28 => res2 = 1 + res2
            case 29 => res2 = 1 + res2
            case 30 => res2 = 1 + res2
            case 31 => res2 = 1 + res2
            case 32 => res2 = 1 + res2
            case 33 => res2 = 1 + res2
            case 34 => res2 = 1 + res2
            case 35 => res2 = 1 + res2
            case 36 => res2 = 1 + res2
            case 37 => res2 = 1 + res2
            case 38 => res2 = 1 + res2
            case 39 => res2 = 1 + res2
            case 40 => res2 = 1 + res2
            case 41 => res2 = 1 + res2
            case 42 => res2 = 1 + res2
            case 43 => res2 = 1 + res2
            case 44 => res2 = 1 + res2
            case 45 => res2 = 1 + res2
            case 46 => res2 = 1 + res2
            case 47 => res2 = 1 + res2
            case 48 => res2 = 1 + res2
            case 49 => res2 = 1 + res2
          }
          i += 1
        }
      }
    }
  }

  performance of "Access" in {
    measure method "Switch" in {
      using(range) in { j =>
        var res2 = 0
        var i = 0
        while (i < nbIters){
          (i % 50: @scala.annotation.switch) match {
            case 0 => res2 = 1 + res2
            case 1 => res2 = 1 + res2
            case 2 => res2 = 1 + res2
            case 3 => res2 = 1 + res2
            case 4 => res2 = 1 + res2
            case 5 => res2 = 1 + res2
            case 6 => res2 = 1 + res2
            case 7 => res2 = 1 + res2
            case 8 => res2 = 1 + res2
            case 9 => res2 = 1 + res2
            case 10 => res2 = 1 + res2
            case 11 => res2 = 1 + res2
            case 12 => res2 = 1 + res2
            case 13 => res2 = 1 + res2
            case 14 => res2 = 1 + res2
            case 15 => res2 = 1 + res2
            case 16 => res2 = 1 + res2
            case 17 => res2 = 1 + res2
            case 18 => res2 = 1 + res2
            case 19 => res2 = 1 + res2
            case 20 => res2 = 1 + res2
            case 21 => res2 = 1 + res2
            case 22 => res2 = 1 + res2
            case 23 => res2 = 1 + res2
            case 24 => res2 = 1 + res2
            case 25 => res2 = 1 + res2
            case 26 => res2 = 1 + res2
            case 27 => res2 = 1 + res2
            case 28 => res2 = 1 + res2
            case 29 => res2 = 1 + res2
            case 30 => res2 = 1 + res2
            case 31 => res2 = 1 + res2
            case 32 => res2 = 1 + res2
            case 33 => res2 = 1 + res2
            case 34 => res2 = 1 + res2
            case 35 => res2 = 1 + res2
            case 36 => res2 = 1 + res2
            case 37 => res2 = 1 + res2
            case 38 => res2 = 1 + res2
            case 39 => res2 = 1 + res2
            case 40 => res2 = 1 + res2
            case 41 => res2 = 1 + res2
            case 42 => res2 = 1 + res2
            case 43 => res2 = 1 + res2
            case 44 => res2 = 1 + res2
            case 45 => res2 = 1 + res2
            case 46 => res2 = 1 + res2
            case 47 => res2 = 1 + res2
            case 48 => res2 = 1 + res2
            case 49 => res2 = 1 + res2
          }
          i += 1
        }
      }
    }
  }


}
