package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c == 0 || c == r then 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def _balance(index: Int, current: Int): Boolean = 
      if index == chars.length then return current == 0
      
      val char = chars(index)
      val newCurrent = char match
        case '(' => current + 1
        case ')' => current - 1
        case _ => current
      newCurrent >= 0 && _balance(index + 1, newCurrent)

    _balance(0, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then return 1
    if coins.isEmpty then return 0

    val coin = coins.head
    val tail = coins.tail
    if coin > money then countChange(money, tail)
    else countChange(money - coin, coins) + countChange(money, tail)
