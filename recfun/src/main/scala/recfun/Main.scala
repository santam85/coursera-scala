package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)

  def pascalTail(c: Int, r: Int): Int = {
    if (c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
    def pascalTailRec(c: Int, r: Int, s1: Int, s2: Int): Int =
      if (c==0 || c==r) 1 else pascalTailRec(c-1,r-1,0,0) + pascal(c,r-1)
    pascalTailRec(c,r,0,0)
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(chars: List[Char], s: Int): Boolean =
      if(s<0) false else
        if(chars.isEmpty) s==0 else
        if(chars.head == '(') balanceRec(chars.tail,s+1) else
        if(chars.head == ')') balanceRec(chars.tail,s-1) else
        balanceRec(chars.tail,s)
    balanceRec(chars,0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0 else
      if (money == 0) 1 else
      if (money<coins.head) countChange(money,coins.tail)
      else countChange(money,coins.tail) + countChange(money-coins.head,coins)
  }
}
