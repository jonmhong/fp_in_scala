package 1datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Branch {
    
    def size[A](count: Int): Int = {

    }

    def maximum()

    def depth()

    def map()

    def fold()

}