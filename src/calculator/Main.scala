package calculator

import scala.io.StdIn.readLine

object Main extends App {
  while (true) {
    val in = readLine().trim

    if (in.nonEmpty) {
      if (in.equalsIgnoreCase("/exit")) {
        println("Bye!")
        System.exit(0)
      } else if (in.equalsIgnoreCase("/help")) {
        println("The program calculates the sum of numbers.")
      } else {
        println(in.split("\\s+").map(v => v.toInt).sum)
      }

    }
  }
}
