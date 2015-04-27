object Exercise2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fib_h(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else if (n == 1) b
      else fib_h(n-1, b, a+b)
    }
    fib_h(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    val msg = "fib(10): %d"
    println(msg.format(fib(10)))
  }
}
