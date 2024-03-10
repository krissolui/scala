def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
    case Nil => Nil
    case y :: ys => 
        if n == 0 then ys
        else y :: removeAt(n - 1, ys)

val xs = List('a', 'b', 'c', 'd')
removeAt(3, xs)

def flatten(xs: Any): List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)
    case _ => xs :: Nil

val ys = List(List(1, 1), 2, List(3, List(5, 8)))
flatten(ys)

extension [T](xs: List[T])
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))

xs.splitAt(2)

// def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
//     def merge[T](xs: List[T], ys: List[T]): List[T] = (xs, ys) match
//         case (Nil, ys) => ys
//         case (xs, Nil) => xs
//         case (x :: xs1, y :: ys1) =>
//             if lt(x, y) then x :: merge(xs1, ys)
//             else y :: merge (xs, ys1)
    
//     val mid = xs.length / 2
//     val (fst, snd) = xs.splitAt(mid)
//     merge(msort(fst)(lt), msort(snd)(lt))

// val v = (1,2)
// v._1

def squareList(xs: List[Int]): List[Int] = xs match
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)

def squareListMap(xs: List[Int]): List[Int] =
    xs.map(x => x * x)

val nums = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
squareList(nums)
squareListMap(nums)

nums.partition(x => x % 2 != 0)
nums.span(x => x % 2 != 0)

def pack[T](xs: List[T]): List[List[T]] = xs match
    case Nil => Nil
    case x :: xs1 => 
        val (same, other) = xs1.span(y => y == x)
        (x :: same) :: pack(other)


val elems = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
pack(elems)

def encode[T](xs: List[T]): List[(T, Int)] = 
    pack(xs).map(x => (x.head, x.length))

encode(elems)

def concat[T](xs: List[T], ys: List[T]): List[T] =
    xs.foldRight(ys)(_ :: _)

val x = List(0, 1, 2, 3, 4)
val y = List(5, 6, 7, 8, 9)
val integers = concat(x, y)

def reverse[T](xs: List[T]): List[T] = 
    xs.foldRight(List[T]())((xs1, x) => x ::: List(xs1))

reverse(integers)

def mapFun[T, U](xs: List[T], f: T => U): List[U] = 
    xs.foldRight(List[U]())(f(_) :: _)

mapFun(integers, x => x * x)

def lengthFun[T](xs: List[T]): Int = 
    xs.foldRight(0)((_, n) => n + 1)

lengthFun(integers)