package reductions

import org.scalameter._
import common._
import scala.annotation._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def loop(chars: Array[Char], acc: Int): Boolean = {
      if (acc < 0) false else
      if (chars.isEmpty) acc == 0 else {
        val h = chars.head
        val t = chars.tail
        val nextAcc = if (h == '(') acc + 1 else if (h == ')') acc - 1 else acc
        loop(t, nextAcc)
      }
    }

    loop(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else {
        val ch = chars(idx)
        if (ch == '(') traverse(idx + 1, until, arg1 + 1, arg2)
        else if (ch == ')') {
          val arg1Dec = arg1 - 1
          traverse(idx + 1, until, arg1Dec, Math.min(arg1Dec, arg2))
        }
        else traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((acc1, min1), (acc2, min2)) = parallel(reduce(from, mid), reduce(mid, until))
        (acc1 + acc2, Math.min(min1, acc1 + min2))
      }
    }

    val (acc, min) = reduce(0, chars.length)
    acc == 0 && min >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
