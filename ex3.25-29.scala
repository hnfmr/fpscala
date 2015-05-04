object Exercise3_25 extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  }

  def maximum(t: Tree[Int]): Int = {
    def helper(tree: Tree[Int], m: Int): Int =
      tree match {
        case Leaf(x) => if (m > x) m else x
        case Branch(l, r) => {
          val lm = helper(l, m)
          val rm = helper(r, m)
          if (lm > rm) lm else rm
        }
      }

    helper(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => {
        val ld = depth(l) + 1
        val rd = depth(r) + 1
        if (ld < rd) rd else ld
      }
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(b: B)(f: (A, B) => B): B = {
    def toList(t: Tree[A], acc: List[A]): List[A] = 
      t match {
        case Leaf(a) => a :: acc
        case Branch(l, r) => toList(l, acc) ++ toList(r, acc)
      }

    toList(t, List[A]()).foldRight(b)(f)
  }

  def foldA[A](t: Tree[A])(f: (A, A) => A): A = {
    t match {
      case Leaf(a) => a
      case Branch(l, r) => f(foldA(l)(f), foldA(r)(f))
    }
  }

  val t1 = Branch(Branch(Leaf(3), Leaf(4)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(5), Leaf(6))))

  val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  println("t1 size: %d".format(size(t1)))
  println("t1 depth: %d".format(depth(t1)))
  println("t1 maximum: %d".format(maximum(t1)))
  println(map(t1)(x => x * 2))
  println(map(t2)(x => x - 1))

  println("fold + : %d".format(fold(t1)(0)(_ + _)))
  println("fold * : %d".format(fold(t2)(1)(_ * _)))

  println("foldA + : %d".format(foldA(t1)(_ + _)))
  println("foldA * : %d".format(foldA(t2)(_ * _)))
}
