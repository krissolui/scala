package week2

val tolerance = 0.001

def abs(x:Double) = if (x < 0) -x else x

def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
    def iterate(guess: Double): Double =
        val next = f(guess)
        if isCloseEnough(guess, next) then next
        else iterate(next)

    iterate(firstGuess)

def averageDamp(f: Double => Double)(x: Double): Double = 
    (x + f(x)) / 2

// def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

@main def test = 
    println(sqrt(2))