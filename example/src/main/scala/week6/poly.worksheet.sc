class Polynom(nonZeroTerms: Map[Int, Double]):
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = nonZeroTerms.withDefaultValue(0.0)

    // def + (other: Polynom): Polynom =
    //     Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

    def + (other: Polynom): Polynom =
        Polynom(other.terms.foldLeft(terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)) = 
        val (exp, coeff) = term
        terms + (exp -> (coeff + terms(exp)))

    override def toString(): String = 
        val termStrings = 
            for (exp, coeff) <- terms.toList.sorted.reverse
                if coeff != 0.0
            yield 
                val exponent = if exp == 0 then "" else s"x^$exp"
                s"$coeff$exponent"
        if terms.isEmpty then "0"
        else termStrings.mkString(" + ")

val x = Polynom(0 -> 2, 1 -> 3, 2 -> 1)
val y = Polynom(0 -> 2, 1 -> 3, 3 -> 4)
val z = Polynom(Map())

x + y + z