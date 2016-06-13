package funsets

object Main extends App {
  import FunSets._
//  println(contains(singletonSet(1), 1))


  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val minS = singletonSet(-100)
  val maxS = singletonSet(100)
  val s5 = union(union(union(union(s1, s2), s3), minS), maxS)
  val fun = (x: Int) => x + 5
  val res = map(s5, fun)


  println(forall(s5, _ >= -100))

  println(exists(s5, _ < -100))

  println(contains(res, 6))




}
