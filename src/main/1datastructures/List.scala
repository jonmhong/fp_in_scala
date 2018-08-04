package 0datastructures

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
            case Cons(_,t) => drop(t, n-1)
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

    /** 
    ex 7: when encountering duplication like with sum() and product(), we can
    generalize by pulling subexpressions out into functional arguments: currying.
    foldRight also replaces the constructors of the list
    */
    def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
        case Nil => z  // base case
        case Cons(h,t) => f(h, foldRight(t, z)(f)) // recursive fn
    }

    def sum2(l: List[Int]) =
        foldRight(l, 0.0)(_ + _)

    def product2(l: List[Double]) = 
        foldRight(l, 1.0)(_ * _)

    /** ex 7: compute the length of a list using foldRight */
    def length[A](l: List[A]): Int =
        foldRight(l, 0)((acc,_) => acc + 1)

    /** ex 10: write a tail-recursive fn and call it foldLeft */
    @annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B) B = l match {
        case Nil => z
        case Cons(h, t) => f(t, foldLeft(h, z)(f))
    }

    /** ex 11: write sum, product, and length fns using foldLeft */
    def sum3[A](l: List[A]): A = {
        foldLeft(l, 0.0)(_ + _)
    }

    def product3[A](l: List[A]): A = {
        foldLeft(l, 1.0)(_ * _)
    }

    def length2[A](l: List[A]): Int = {
        foldLeft(l, 0.0)((acc,_) => acc + 1)
    }

    /** ex 12: write a fn that returns the reverse of the list, using a fold */
    def reverseFoldLeft[A](l: List[A]): List[A] = {
        foldLeft(l, List[A]())((t,h) => Cons(h, t))
    }

    def reverseFoldRight[A](l: List[A]): List[A] = {
        foldRight(l, List[A]())((h, t) => Cons(t, h))
    }

    /**
    ex 13(hard): write foldLeft in terms of foldRight; write vice versa
    (This is a common trick to avoid stack overflow)
    */
    def foldLeftViaFoldRight[A,B](l: List[A], z: B): B = {
        foldRight(reverse(l), z)((a,b) => f(a,b))
    }

    def foldRightViaFoldLeft[A,B](l: List[A], z: B): B = {
        foldLeft(reverse(l), z)((b,a) => f(a,b))
    }

    /** ex 14(hard): implement append in terms of foldLeft and foldRight */
    def appendLeft[A](l: List[A], v: List[A]): List[A] = {
        foldLeft(l, v)(Cons(_, _))
    }

    def appendRight[A](l: List[A], v: List[A]): List[A] = {
        foldRight(l, v)(Cons(_, _))
    }

    /**
    ex 15(hard): write a fn that concats a list of lists into a single list.
    runtime should be linear with respect to the total len of all lists.
    use foldLeft and foldRight.
    */
    def concat[A](l: List[List[A]]): List[A] = {
        foldRight(l, Nil:List[A])(appendRight)
    }

    /** ex 16: write a fn that transforms a list of ints by adding to to each element */
    def addOne(l: List[Int]): List[Int] = {
        foldRight(l, Nil:List[Int])((h,t) => Cons(h+1, t))
    }

    /** ex 17: write a fn that turns each value in a List[Double] into a String */
    def stringify(l: List[Double]): List[String] = {
        foldRight(l, Nil:List[String])((h,t) => Cons(h.toString, t))
    }

    /**
    ex 18: write a fn map that generalized modifying each element while maintaining 
    the structure of the list
    */
    def map[A,B](l: List[A])(f: A => B): List[B] = {
        foldRight(l, Nil:List[B])((h,t) => Cons(f(h), t))
    }

    // use foldRightViaFoldLeft to avoid building a high stack
    def map2[A,B](l: List[A])(f: A => B): List[B] = {
        foldRightViaFoldLeft(l, Nil:List[B])(((h, t) => Cons(f(h), t)))
    }

    /**
    ex 19: write a function filter that removes elements from a list unless they
    satisfy a given predicate. Use it to remove all odd numbers from a List[Int]
    */
    def filter[A](l: List[A])(f: A => Boolean): List[A] = {
        foldRight(l, Nil:List[A])((h,t) => if f(h) Cons(h, t) else t)
    }

    /** ex 20: write a function flatMap that works like map except that the function will
    return a list instead of a single result
    */
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
        concat(map(l)(f))
    }

    // use foldRight
    def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = {
    }

    /** ex 21: can you use flatMap to implement filter */
    def flatMap[A,B](l: List[A]])(f: A => List[B]): List[B] = {}
        filter()

    /** 
    ex 22: write a function that accepts two lists and constructs a new list by adding 
    corresponding elements. ([a + b for a, b in zip(ListA, ListB)])
     */
     def addZip(): = {}

    /** 
    ex 23: generalize the function you just wrote so that it's not specific to
    integers or addition
    */
    def zip[]()(): = {}

    /**
    ex 24: implement hasSubsequence for checking whether a List contains another List
    */
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {}

    /**
    For standard library functions:
    https://www.scala-lang.org/api/current/scala/collection/immutable/List.html
    */
}
