/** These functions are added, so we can called them with obj.fn(arg1) rather than fn(obj, arg1) */
trait Option[+A] {
    // sealed is removed when adding these functions
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
}
class object Some[+A](get: A): extends Option[A]
object None extends Option[Nothing]

