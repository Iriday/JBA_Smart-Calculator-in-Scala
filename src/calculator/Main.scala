package calculator

import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  run()

  def run() {
    while (true) {
      val in = getInput

      if (in.nonEmpty) {
        val errorMsg = checkInput(in)
        if (errorMsg != null) {
          output(errorMsg)
        } else if (in.equals("/exit")) {
          output("Bye!")
          System.exit(0)
        } else if (in.equals("/help")) {
          output("Example: 2+2 -2 ---2 +-2")
        } else {
          output(solveExpression(formatExpression(in)))
        }
      }
    }
  }

  def getInput: String = readLine().trim.toLowerCase

  def output(data: Any): Unit = println(data)

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

  /**
   * @param in input
   * @return null if input is correct else error msg
   */
  def checkInput(in: String): String = {
    // general
    if (in.isEmpty) return null
    // command
    if (in.startsWith("/")) {
      if (in.equals("/help") || in.equals("/exit")) return null
      else return "Unknown command"
    }
    // expression
    if (".*[0-9]\\s+[0-9].*".r.matches(in)) return "Invalid expression"

    val in2 = in.replace(" ", "") // remove whitespaces to simplify regex
    if (!"[-+]*[0-9]+([-+]+[0-9]+)*".r.matches(in2)) return "Invalid expression"

    null
  }
}
