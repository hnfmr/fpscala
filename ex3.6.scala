object Exercise3_6 extends App {
  def init[A](ls: List[A]): List[A] = {
    def helper(acc: List[A], as: List[A]): List[A] = {
      as match {
        case Nil => acc
        case h::t => match t {
                      
                     }
      }
    }
    ls match {
      case Nil => Nil
      case h::t => if (f(h)) dropWhile(f, t) else ls
    }
  }

  val ls = List(5,4,3,2,1)
  println(dropWhile( (x:Int) => if (x > 3) true else false, ls).mkString(","))
}
