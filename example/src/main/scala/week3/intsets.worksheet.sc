abstract class IntSet:
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(s: IntSet): IntSet
end IntSet

object IntSet:
    def apply(): IntSet = Empty
    def apply(x: Int): IntSet = Empty.incl(x)
    def apply(x: Int, y: Int): IntSet = Empty.incl(x).incl(y)
end IntSet

object Empty extends IntSet:
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    def union(s: IntSet): IntSet = s
end Empty

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def contains(x: Int): Boolean = 
        if x == elem then true
        else if x < elem then left.contains(x)
        else right.contains(x)

    def incl(x: Int): IntSet = 
        if x == elem then this
        else if x < elem then NonEmpty(elem, left.incl(x), right)
        else NonEmpty(elem, left, right.incl(x))

    def union(s: IntSet): IntSet = 
        left.union(right).union(s).incl(elem)
end NonEmpty