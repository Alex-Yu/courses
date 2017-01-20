package calculator

import scala.annotation.tailrec

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  private def getRefNames(expr: Expr): List[String] = {
    def loop(e: Expr, z: List[String]): List[String] = e match {
      case Literal(_) => z
      case Ref(name) => name :: z
      case Plus(a, b) => loop(a, z) ::: loop(b, z)
      case Minus(a, b) => loop(a, z) ::: loop(b, z)
      case Times(a, b) => loop(a, z) ::: loop(b, z)
      case Divide(a, b) => loop(a, z) ::: loop(b, z)
    }
    loop(expr, List.empty)
  }

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val validRefs = getValid(namedExpressions)

    namedExpressions.map { case (name, sigE) =>
      name -> Signal (
        if (validRefs.contains(name)) eval(sigE(), namedExpressions)
        else Double.NaN
      )
    }
  }

  private def getValid(all: Map[String, Signal[Expr]]): Set[String] = {
    val nonValid = all.collect {
      case p@(name, sE) if !isValid(name, sE(), all) => name
    }.toSet

    def loop(z: Map[String, Signal[Expr]], nonValid: Set[String]): Set[String] = {
      val _nonValid = z.collect {
        case p@(name, sE) if (getRefNames(sE()).toSet intersect nonValid).nonEmpty => name
      }.toSet
      if (_nonValid.nonEmpty) loop(z -- _nonValid, _nonValid) else z.keySet
    }

    loop(all -- nonValid, nonValid)
  }

  private def isValid(n: String, expr: Expr, all: Map[String, Signal[Expr]]): Boolean = {
    val refNames = getRefNames(expr)
    val allNames = all.keySet
    val isProperRefs = refNames.forall(name => allNames.contains(name))
    val isNonCyclic = refNames.forall { eN =>
        val e = all(eN)()
        !getRefNames(e).contains(n)
      }
    isProperRefs && isNonCyclic
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def loop(e: Expr, z: Double): Double = e match {
      case Literal(v) => z + v
      case Ref(name) =>
        val refExpr = references(name)()
        loop(refExpr, z)
      case Plus(a, b) => loop(a, z) + loop(b, z)
      case Minus(a, b) => loop(a, z) - loop(b, z)
      case Times(a, b) => loop(a, z) * loop(b, z)
      case Divide(a, b) => loop(a, z) / loop(b, z)
    }
    loop(expr, 0)
  }




  /** Get the Expr for a referenced variables.
    *  If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
