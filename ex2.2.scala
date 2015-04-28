object Exercise2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    val sorted = as.sortWith(ordered)
    sorted.deep == as.deep
  }

  def main(args: Array[String]): Unit = {
    val as = Array(1,5,4,6)
    val comp = (x:Int,y:Int) => x < y

    println(isSorted(as, comp))
    
    val bs = Array(1,2,3,4,5)
    println(isSorted(bs, comp))

  }
}
