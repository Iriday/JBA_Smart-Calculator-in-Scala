package calculator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object InfixPostfixConverter {
  def infixToPostfix(expr: ArrayBuffer[String]): ArrayBuffer[String] = {
    val stack = new mutable.Stack[String]
    val result = new ArrayBuffer[String]()

    for (v <- expr) {
      if (v.equals("(")) {
        stack.push(v)
      }
      else if (v.equals(")")) {
        var elem = stack.pop()
        while (!elem.equals("(")) {
          result += elem
          elem = stack.pop()
        }
      } else if (v.equals("+") || v.equals("-")) {
        if (stack.nonEmpty && (stack.head.equals("*") || stack.head.equals("/"))) result += stack.pop()
        if (stack.nonEmpty && (stack.head.equals("-") || stack.head.equals("+"))) result += stack.pop()
        stack.push(v)
      } else if (v.equals("*") || v.equals("/")) {
        if (stack.nonEmpty && (stack.head.equals("*") || stack.head.equals("/"))) result += stack.pop()
        stack.push(v)
      } else { // number
        result += v
      }
    }
    stack.foreach(v => result += v)

    result
  }
}
