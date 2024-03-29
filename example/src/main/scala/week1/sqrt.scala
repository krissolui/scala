package week1

def squre(x:Double) = x * x
def abs(x:Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
        if (isGoodEnough(guess)) guess
        else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = 
        abs(x / guess - guess) < 0.001

    def improve(guess: Double) =(guess + x / guess) / 2

    sqrtIter(1.0)
}

// 0.001

// 0.1e-20

// 1.0e20

// 1.0e50

@main def test = println(sqrt(2))