package calculator

import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  while (true) {
    val in = readLine().trim

    if (in.nonEmpty) {
      if (in.equalsIgnoreCase("/exit")) {
        println("Bye!")
        System.exit(0)
      } else if (in.equalsIgnoreCase("/help")) {
        println("Example: 2+2 -2 ---2 +-2")
      } else {
        println(solveExpression(formatExpression(in)))
      }
    }
  }

  // removes whitespaces, and merges operators
  def formatExpression(expression: String): ArrayBuffer[String] = {
    def mergeOperators(a: Char, b: Char): Char = if (a == '-' && b == '-' || a == '+' && b == '+') '+' else '-'

    val expr = expression.replace(" ", "")
    val exprArr = ArrayBuffer.empty[String]

    var i = 0
    while (i < expr.length) {
      // number
      if (expr(i).isDigit) {
        val numStartPoz = i
        i += 1
        while (i < expr.length && expr(i).isDigit) {
          i += 1
        }
        exprArr += expr.substring(numStartPoz, i)
      }
      // operators
      else if ("-+".contains(expr(i))) {
        var operator = expr(i)
        i += 1
        while ("-+".contains(expr(i))) {
          operator = mergeOperators(operator, expr(i))
          i += 1
        }
        exprArr += operator.toString
      }
      // incorrect expression
      else {
        throw new IllegalArgumentException("Error: expression is incorrect")
      }
    }

    exprArr
  }

  def solveExpression(expr: ArrayBuffer[String]): Int = {
    if (!"-".equals(expr(0)) && !"+".equals(expr(0))) expr.insert(0, "+")
    var result = 0
    var i = 0

    while (i < expr.length) {
      if ("-" == expr(i)) {
        result = result - expr(i + 1).toInt
      } else if ("+".equals(expr(i))) {
        result = result + expr(i + 1).toInt
      } else {
        throw new IllegalArgumentException("Error: unknown operator")
      }
      i += 2
    }

    result
  }
}
