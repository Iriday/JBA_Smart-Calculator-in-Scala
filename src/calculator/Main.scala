package calculator

import scala.io.StdIn.readLine
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import InfixPostfixConverter._

object Main extends App {
  private val variables: mutable.HashMap[String, String] = mutable.HashMap()

  private val rLetters = "[a-zA-Z]+".r
  private val rNumWithOptionalSign = "[-+]?[0-9]+".r
  private val rWhitespaceBetweenAlphanum = ".*[0-9a-zA-Z]\\s+[0-9a-zA-Z].*".r
  private val rExpression = "[-+]*([0-9]+|[a-zA-Z]+)(([-+]+|[*/^][-+]?)([0-9]+|[a-zA-Z]+))*".r // remove whitespaces/parenthesis before check
  private val rIncorrectParenthesis = "(.*[^-+/*^(][(].*)|(.*[)][^-+/*^)].*)|(.*[(][^-+(0-9a-zA-Z].*)|(.*[^0-9a-zA-Z][)].*)|([-+]+[(].*)".r

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
          output(if (errorMsg != null) errorMsg else solveExpression(infixToPostfix(formatExpression(in, variables))))
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
        val s = i // number start position
        i += 1
        while (i < expr.length && expr(i).isDigit) {
          i += 1
        }
        val number = expr.substring(s, i)
        if (s != 0 && "-+".contains(expr(s - 1)) && (s - 1 == 0 || "*/(^".contains(expr(s - 2))))
          exprArr(exprArr.size - 1) = exprArr.last + number
        else
          exprArr += number
      }
      // + - operator
      else if ("-+".contains(expr(i))) {
        var operator = expr(i)
        i += 1
        while ("-+".contains(expr(i))) {
          operator = mergeOperators(operator, expr(i))
          i += 1
        }
        exprArr += operator.toString
      }
      // parenthesis and single operators
      else if ("()*/^".contains(expr(i))) {
        exprArr += expr(i).toString
        i += 1
      }
      // incorrect expression
      else {
        throw new IllegalArgumentException("Error: expression is incorrect")
      }
    }

    if (exprArr(0).equals("+") || exprArr(0).equals("-")) {
      val sign = exprArr.remove(0)
      exprArr(0) = sign + exprArr(0)
    }

    exprArr
  }

  def solveExpression(expr: ArrayBuffer[String]): Int = {
    val stack = new mutable.Stack[Int]()

    for (v <- expr) {
      if ("-+*/^".contains(v)) {
        val b = stack.pop()
        val a = stack.pop()

        if ("-".equals(v)) stack.push(a - b)
        else if ("+".equals(v)) stack.push(a + b)
        else if ("*".equals(v)) stack.push(a * b)
        else if ("/".equals(v)) stack.push(a / b)
        else if ("^".equals(v)) stack.push(scala.math.pow(a, b).toInt)

      } else {
        stack.push(v.toInt)
      }
    }

    if (stack.length != 1) throw new IllegalArgumentException("Error: expression is incorrect")

    stack.pop()
  }

  private def executeCommand(in: String) {
    if (in.equals("/exit")) {
      output("Bye!")
      System.exit(0)
    } else if (in.equals("/help")) {
      output("Examples: a = 2; b=3; ((a*2) +-2 ---b + ((+3) * 10)  /2)^(1+1)")
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
    if (rIncorrectParenthesis.matches(expr2)) return "Invalid expression"
    val parenthesis = expr2.replaceAll("[^()]+", "")
    if (parenthesis.length % 2 != 0 || parenthesis.replace("(", "").length != parenthesis.length / 2)
      return "Invalid expression"

    val expr3 = expr2.replace("(", "").replace(")", "") // remove parenthesis to simplify regex
    if (!rExpression.matches(expr3)) return "Invalid expression"

    val exprVars = expr.split("[^a-zA-Z]+")
    exprVars.foreach(v => if (!vars.contains(v) && v != "") return "Unknown variable")

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
