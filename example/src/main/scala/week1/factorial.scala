package week1

def factorial(x: Int) =
    def loop(x: Int, acc: Int): Int =
        if x == 0 then acc
        else loop(x - 1, x * acc)

    loop(x, 1)
