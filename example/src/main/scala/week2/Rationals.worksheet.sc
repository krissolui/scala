class Rational(x: Int, y: Int):
    require(y > 0, s"denominator must be positive, was $x/$y")
    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
        if b == 0 then a else gcd(b, a % b)
    private val g = gcd(x.abs, y)

    def numer = x
    def denom = y

    def neg = Rational(-numer, denom)

    def add(r: Rational) =
        Rational(numer * r.denom + r.numer * denom,
            denom * r.denom)

    def sub(r: Rational) =
        add(r.neg)

    def mul(r: Rational) =
        Rational(numer * r.numer, denom * r.denom)

    def less(that: Rational): Boolean =
        this.numer * that.denom < that.numer * this.denom
    
    def max(that: Rational): Rational =
        if this.less(that) then that else this

    override def toString(): String = s"${numer / g}/${denom / g}"
end Rational

extension (r: Rational)
    def min(s: Rational): Rational = 
        if s.less(r) then s else r

    def abs: Rational = Rational(r.numer.abs, r.denom)

    def +(s: Rational): Rational = r.add(s)
    def -(s: Rational): Rational = r.sub(s)
    def *(s: Rational): Rational = r.mul(s)

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)
y.min(z)

(x + y) * z

def a: Int = 0
def b: Int = 0
def c: Int = 0
def d: Int = 0

// a + b ^? c ?^ d less a ==> b | c
// ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)