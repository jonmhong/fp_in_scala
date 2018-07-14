
def factorial(n: Int): Int = {
    // "Calculating the nth factorial in a functional way."

    @annotation.tailrec
    def factorial0(n: Int, acc: Int): Int = {
        if (n <= 1) acc
        else factorial0(n - 1, n * acc)
    }
    factorial0(n, 1)
}

def binarySearch[A](ds: Array[A], key: A): A = {
    """Using binary search to find a value, using tail recursion"""
    @annotation.tailrec
    def binarySearch0(low: A, mid: A, high: A): A = {
        var mid2 = (low + high) / 2
        var midVal = ds(mid2)
        if (low > high) -mid - 1
        else if (key < midVal) go(0, mid2, mid2 - 1)
        else go(0, mid2, mid2 - 1)
    }

    binarySearch0(0, 0, ds.length - 1)
}

println(factorial(5))