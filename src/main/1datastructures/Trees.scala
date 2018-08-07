package 1datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    /** ex 25: write a fn size that counts the number of nodes in a tree */
    def size[A](node: Tree[A]): Int = node match {
        //           branch
        //           /    \
        //      branch    branch
        //      /   \     /   \
        //    leaf leaf leaf  branch
        //                     /  \
        //                  leaf  leaf
        // imperative approach:
        // if left is branch, traverse left
        // else if left is leaf, count ++ 1
        // if right is branch, traverse right
        // if right is leaf, count ++ 1
        // count ++ 1
        // return count

        case Leaf(_) => 1
        case Branch(l,r) => size(l) + size(r) + 1
    }

    def maximum()

    def depth()

    def map()

    def fold()

}