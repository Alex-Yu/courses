val pattern = "(t[0-9]{2,4}).*".r

val a = "t123dnkdfk"

a match {
  case pattern("t12") => "+"
  case _ => "-"
}