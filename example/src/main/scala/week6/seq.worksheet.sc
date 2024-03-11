def isPrime(n: Int): Boolean =
    !(2 until n).exists( n % _ == 0)

isPrime(17)
isPrime(19)
isPrime(21)
isPrime(32)
isPrime(97)

def primePairs(n: Int): Seq[(Int, Int)] =
    // (1 until n)
    //     .flatMap(i => (1 until i).map(j => (1, j)))
    //     .filter((x, y) => isPrime(x + y))

    for 
        i <- 1 until n
        j <- 1 until i
        if isPrime(i + j)
    yield (i, j)

primePairs(17)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for (x, y) <- xs.zip(ys) yield x * y).sum

def queens(n: Int) = 
    def placeQueens(k: Int): Set[List[Int]] =
        if k == 0 then Set(List())
        else 
            for
                queens <- placeQueens(k -1)
                col <- 0 until n
                if isSafe(col, queens)
            yield col :: queens
    
    placeQueens(n)

def isSafe(col: Int, queens: List[Int]): Boolean = 
    !checks(col, 1, queens)

def checks(col: Int, delta: Int, queens: List[Int]): Boolean = queens match
    case qcol :: others => 
        qcol == col
        || (qcol - col).abs == delta
        || checks(col, delta + 1, others)
    case Nil => false

queens(5)