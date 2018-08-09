package 1datastructures

import scala.math.max

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    /** ex 25: write a fn size that counts the number of nodes in a tree */
    def size[A](node: Tree[A]): Int = node match {
        //           branch
        //          /      \
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

    /** ex 26: write a fn maximum that returns the maximum element in a tree */
    def maximum[A](node: Tree[A]): A = node match {
        case Leaf(v) => v
        case Branch(l,r) => max(maximum(l), maximum(r))
        // can also be written like this:
        // case Branch(l,r) => maximum(l) max maximum(r)
    }

    /** ex 27: write a fn depth that returns the max path length from the root to leaf*/
    def depth[A](node: Tree[A]): Int = node match {
        case Leaf(_) => 0
        case Branch(l,r) => max(depth(l) + 1, depth(r) + 1)
    }

    /** ex 28: write a fn map that modifies each element in a tree with a given fn */
    def map[A,B](node: Tree[A])(f: A => B): Tree[B] = node match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

    /** ex 29: write a new fold function that abstracts over the others' similarities */
    def fold[A,B](node: Tree[A])(f: A => B)(g: (B,B) => B): B = node match {
        case Leaf(v) => f(v)
        case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](root: Tree[A]): Int = {
        fold(root)(_ => 1)(_ + _ + 1)
    }

    def maximumViaFold[A](root: Tree[A]): A = {
        fold(root)(v => v)((l,r) => max(l, r))
    }

    def depthViaFold[A](root: Tree[A]): Int = {
        fold(root)(_ => 0)((l,r) => 1 + max(l, r))
    }

    def mapViaFold[A,B](root: Tree[A])(f: A => B): Tree[B] = {
        fold(root)(v => Leaf(f(a)): Tree[B])(Branch(_,_))
    }
}
