package datastructures

// trait: abstract interface that contains implementations of some methods
// sealed: all implementations can only be declared within this file
// +A: declares the List data type to be polymorphic of the elements it contains
sealed trait List[+A]

// two implementations of data constructors of List, each intro'd with "case"
// Nil: empty
case object Nil extends List[Nothing]
// Cons: short for "construct", which consists of at least an initial element
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// companion object: in addition to creating our data type of the same name
// we add functions for creating or working with values of the data type
object List {
    def sum(ints: List[Int]): Int = ints match {
        // Case of an empty List
        case Nil => 0
        // else recursively calculate its sum
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 0.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    // start jonhong
    def tail[A](ds: List[A]): A = ds match {
        case Nil => ds
        case Cons(x, xs) => xs
    }

    def drop[A](ds: List[A], n: Int): Unit = {
        @annotation.tailrec
        def drop0(ds0: List[A], acc: Int): A = {
            if (ds0 == Nil) ds0
            else if (acc == 0) ds0
            else {
                case Cons(x, xs) => xs
                drop0(xs, n - 1)
            }
        }

        drop0(ds, n)
    }

    def setHead[A](ds0: List[A], val: A): List[A] = match ds0 {
        case Nil => ds0
        case Cons(x, _) => Cons(val, _)
    }

    def extendList[A](a1: List[A], a2: List[A]): List[A] = match a1 {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
    }

    def init[A](l: List[A]): List[A] = {
        @annotation.tailrec
        def init0[A](l): List[A] = match l {
            var h,t = Cons(h,t)
            if (t == Nils)
        }

        init0()
    }
    // end jonhong

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
        // this doesn't make sense. need to go over this slowly
        case Nil => z
        case Cons(h, t) => f(x, foldRight(h, z)(f))
    }

    def sum2(l: List[Int]) =
        foldRight(1, 0.0)(_ + _)

    def product2(l: List[Double]) = 
        foldRight(1, 1.0)(_ * _)

    
}
