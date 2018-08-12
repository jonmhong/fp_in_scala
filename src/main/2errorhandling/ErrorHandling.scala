/** These functions are added, so we can called them with obj.fn(arg1) rather than fn(obj, arg1) */
// sealed is removed when adding these functions
trait Option[+A] {
    /** ex 1: implement all the above functions on Option*/
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    // (default: => B) syntax: the fn will not be evaluated until needed by the fn
    def getOrElse[B >: A](default: => B): B = {
        case None => default
        case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        map(f) getOrElse None
    }

    // [B >: A] syntax: indicates that B must be a supertype of A
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this map (Some(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
        case Some(a) if f(a) => this
        case _ => None
    }

}
class object Some[+A](get: A): extends Option[A]
object None extends Option[Nothing]
