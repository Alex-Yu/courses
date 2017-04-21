val a = List(1, 2)

def to(x: Int*) = x.map(_ + 1)

to(a: _*)