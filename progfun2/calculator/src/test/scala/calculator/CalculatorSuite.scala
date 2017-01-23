package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import TweetLength.MaxTweetLength
import Calculator._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
    ** TWEET LENGTH **
    ******************/

  def tweetLength(text: String): Int =
  text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a dynamic signal") {
    val in = Var("hello")
    val result = TweetLength.tweetRemainingCharsCount(in)
    assert(result() == MaxTweetLength - tweetLength("hello"))

    in() = "hello world"
    val result2 = TweetLength.tweetRemainingCharsCount(in)
    assert(result2() == MaxTweetLength - tweetLength("hello world"))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  ignore("compute values") {
    val a: Var[Expr] = Var(Literal(10d))
    val b: Var[Expr] = Var(Ref("a"))
    val c: Var[Expr] = Var(Plus(Literal(5d), Ref("b")))

    val all: Map[String, Signal[Expr]] = Map(
      "a" -> a.asInstanceOf[Signal[Expr]],
      "b" -> b.asInstanceOf[Signal[Expr]],
      "c" -> c.asInstanceOf[Signal[Expr]]
    )

    val result1 = computeValues(all)
    assert(result1("a")() == 10d)
    assert(result1("b")() == 10d)
    assert(result1("c")() == 15d)
  }

  test("compute values with NaN") {
    val a: Var[Expr] = Var(Ref("b"))
    val b: Var[Expr] = Var(Ref("a"))
    val c: Var[Expr] = Var(Ref("c"))

    val all: Map[String, Signal[Expr]] = Map(
      "a" -> a.asInstanceOf[Signal[Expr]],
      "b" -> b.asInstanceOf[Signal[Expr]],
      "c" -> c.asInstanceOf[Signal[Expr]]
    )

    val result1 = computeValues(all)
    assert(result1("a")().isNaN)
    assert(result1("b")().isNaN)
    assert(result1("c")().isNaN)
  }

}
