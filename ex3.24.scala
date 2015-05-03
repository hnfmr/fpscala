object Exercise3_24 extends App {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (sub.length > sup.length) false
    else if (sub.length == sup.length) sub == sup
    else {
      val f = sup take sub.length
      if (f == sub) true
      else hasSubsequence(sup.tail, sub)
    }
  }
}
