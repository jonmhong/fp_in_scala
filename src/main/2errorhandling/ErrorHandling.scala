import math

/** These functions are added, so we can called them with obj.fn(arg1) rather than fn(obj, arg1) */
// sealed is removed when adding these functions
trait Option[+A] {
    /** ex 1: implement all the above functions on Option*/
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    // (default: => B) syntax: the fn will not be evaluated until needed by the fn
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        map(f) getOrElse None
    }

    // implementation using pattern matching
    def flatMap2[B](f: A => B): Option[A] = this match {
        case None => None
        case Some(a) => f(a)
    }

    // [B >: A] syntax: indicates that B must be a supertype of A
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this map (Some(_)) getOrElse ob
    }

    // implementation using pattern matching
    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if f(a) => this
        case _ => None
    }

    // implementation using flatMap
    def filter2(f: A => Boolean): Option[A] = {
        flatMap(a => if f(a) Some(a) else None)
    }

    /** ex 2: implement the variance function, (variance is the mean of math.pow(x - m, 2))
    */
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs) getOrElse (m => mean(xs.map(x => math.pow(x - m, 2))))
    }
}

class object Some[+A](get: A): extends Option[A]
object None extends Option[Nothing]
