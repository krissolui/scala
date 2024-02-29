trait LIST[T]:
    def isEmpty: Boolean
    def head: T
    def tail: LIST[T]
end LIST

class CONS[T](val head: T, val tail: LIST[T]) extends LIST[T]:
    def isEmpty = false
end CONS

class NIL[T] extends LIST[T]:
    def isEmpty = true
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.tail")
end NIL

def nth[T](xs: LIST[T], n: Int): T =
    if xs.isEmpty then throw IndexOutOfBoundsException()
    else if n == 0 then xs.head
    else nth(xs.tail, n - 1)