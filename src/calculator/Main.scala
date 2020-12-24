package calculator

import scala.io.StdIn.readLine
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  private val variables: mutable.HashMap[String, String] = mutable.HashMap()

  private val rLetters = "[a-zA-Z]+".r
  private val rNumWithOptionalSign = "[-+]?[0-9]+".r
  private val rWhitespaceBetweenAlphanum = ".*[0-9a-zA-Z]\\s+[0-9a-zA-Z].*".r
  private val rExpression = "[-+]*([0-9]+|[a-zA-Z]+)([-+]+([0-9]+|[a-zA-Z]+))*".r

  run(variables)

  def run(variables: mutable.HashMap[String, String]) {
    while (true) {
      val in = getInput

      if (in.nonEmpty) {
        if (in.startsWith("/")) {
          executeCommand(in)
        } else if (in.contains("=")) {
          val errorMsg = checkAssignment(in, variables)
          if (errorMsg != null) output(errorMsg) else addVariable(in)
        } else {
          val errorMsg = checkExpression(in, variables)
          output(if (errorMsg != null) errorMsg else solveExpression(formatExpression(in, variables)))
        }
      }
    }
  }

  def getInput: String = readLine().trim

  def output(data: Any): Unit = println(data)

  /**
   * Remove whitespaces, merge operators (- and +), replace vars names with values.
   */
  def formatExpression(expression: String, variables: mutable.HashMap[String, String]): ArrayBuffer[String] = {
    def mergeOperators(a: Char, b: Char): Char = if (a == '-' && b == '-' || a == '+' && b == '+') '+' else '-'

    var expr = expression.replace(" ", "")
    variables.foreach(v => expr = expr.replace(v._1, v._2))
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

  private def executeCommand(in: String) {
    if (in.equals("/exit")) {
      output("Bye!")
      System.exit(0)
    } else if (in.equals("/help")) {
      output("Examples: a = 2; b=3; a+2 -2 ---b +-2")
    } else {
      output("Unknown command")
    }
  }

  private def addVariable(in: String) {
    val v = in.split(" *= *")
    variables += v(0) -> variables.getOrElse(v(1), v(1))
  }

  // Input check

  /**
   * @return null if expression is correct else error msg
   */
  private def checkExpression(expr: String, vars: mutable.HashMap[String, String]): String = {
    if (rWhitespaceBetweenAlphanum.matches(expr)) return "Invalid expression"

    val expr2 = expr.replace(" ", "") // remove whitespaces to simplify regex
    if (!rExpression.matches(expr2)) return "Invalid expression"

    val expr3 = expr2.split("[^a-zA-Z]+")
    expr3.foreach(v => if (!vars.contains(v)) return "Unknown variable")

    null
  }

  /**
   * @return null if assignment is correct else error msg
   */
  private def checkAssignment(a: String, vars: mutable.HashMap[String, String]): String = {
    val a2 = a.split(" *= *")

    if (a2.length != 2) return "Invalid assignment"

    if (!rLetters.matches(a2(0))) return "Invalid identifier"

    if (!rLetters.matches(a2(1)) && !rNumWithOptionalSign.matches(a2(1))) return "Invalid assignment"

    if (rLetters.matches(a2(1)) && !vars.contains(a2(1))) return "Unknown variable"

    null
  }
}
