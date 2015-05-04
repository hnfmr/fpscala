import fpscala.errorhandling._

object Exercise4_2 extends App {
  def variance(xs: Seq[Double]): Option[Double] = {
    def calcVar(xs: Seq[Double]): Double = {
      val m = xs.sum / xs.length
      xs.map(x => math.pow(x - m, 2)).sum / xs.length
    }
    Some(xs).flatMap(x => if (x.isEmpty) None else Some(calcVar(x)))
  }

  val ls = List[Double](1.0,2.0,3.0,4.0,5.0)
  println(variance(ls))

  val els = List()
  println(variance(els))
}
