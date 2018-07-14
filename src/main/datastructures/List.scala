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
    // match: indicates we're using case to check for pattern matching
    def sum(ints: List[Int]): Int = ints match {
        // Case of an empty List
        case Nil => 0
        // else recursively calculate its sum
        case Cons(h,t) => h + sum(t)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 0.0
        case Cons(0.0, _) => 0.0
        case Cons(h,t) => h * product(t)
    }

    /** ex 2: "remove" the first element in a list */
    def tail[A](ds: List[A]): List[A] = ds match {
        case Nil => ds
        case Cons(h,t) => t
    }

    /** ex 3: "remove" the first n elements in a list */
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n <= 0) l
        else l match {
            case Nil => l
            case Cons(_,t) => drop(t, n - 1)
        }
    }

    /** ex 4: "removes" elements from List as long as they match a predicate */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(h,t) if f(h) => dropWhile(t, f)
        case _ => l
    }

    /** ex 5: "replace" the first element of a List with a different value */
    def setHead[A](ds0: List[A], v: A): List[A] = match ds0 {
        case Nil => ds0
        case Cons(_,t) => Cons(v, t)
    }

    /** ex 6: return a List consisting of all but the last element of a List */
    def init[A](l: List[A]): List[A] = l match {
        case Nil => l
        case Cons(h, t) if (t.length == 1) => h
    }
    def init2[A](l: List[A]): List[A] = l match {
        case Nil => l
        case Cons(h,t) => Cons(h,init(t))
    }
    /**
    very large lists will lead to stack overflow. here, a temporary mutable buffer
    is used within the function
    */
    def init3[A](l: List[A]): List[A] = {
        import collection.mutable.ListBuffer
        val buf = new ListBuffer[A]

        @annotation.tailrec
        def go(cur: List[A]): List[A] = l match {
            case Nil => sys.error("init of an empty List")
            case Cons(_,Nil) => List(buf.toList: _*)
            case Cons(h,t) => buf += h; go(t)
        }

        go(l)
    }

    def extendList[A](a1: List[A], a2: List[A]): List[A] = match a1 {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
        // this doesn't make sense. need to go over this slowly
        case Nil => z
        case Cons(h,t) => f(x, foldRight(h, z)(f))
    }

    def sum2(l: List[Int]) =
        foldRight(1, 0.0)(_ + _)

    def product2(l: List[Double]) = 
        foldRight(1, 1.0)(_ * _)

    
}
