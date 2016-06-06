package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else {
      val upperR = r - 1
      pascal(c - 1, upperR) + pascal(c, upperR)
    }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], acc: Int): Boolean = {
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

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
