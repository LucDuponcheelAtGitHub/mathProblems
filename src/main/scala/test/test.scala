package test

import scala.io.AnsiColor.*

val test: String => Boolean => Unit =
  message =>
    booleanResult =>
      print(s"$message")
      if (booleanResult) {
        println(s" is ${GREEN}OK${RESET}")
      } else {
        println(s" is ${RED}KO${RESET}")
      }
