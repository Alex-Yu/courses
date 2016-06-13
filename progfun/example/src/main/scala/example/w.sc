def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, 1, _ * _)(a, b)

def factorial(n: Int) = mapReduce(identity, 1, _ * _)(1, n)


def mapReduce(f: Int => Int, zero: Int, combine: (Int, Int) => Int)(a: Int, b: Int): Int =
if (a > b) zero else combine(f(a), mapReduce(f, zero, combine)(a + 1, b))
