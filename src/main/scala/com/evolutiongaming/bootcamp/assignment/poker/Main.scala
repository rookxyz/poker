package com.evolutiongaming.bootcamp.assignment.poker

import scala.io.StdIn
import com.evolutiongaming.bootcamp.assignment.poker.Solver.process

object Main {
  def main(args: Array[String]): Unit = {

//    Iterator.continually(Option(StdIn.readLine()))
//      .takeWhile(_.nonEmpty)
//      .foreach { x =>
//        x map process foreach println
//      }
    Iterator.continually(Option(StdIn.readLine()))
      .takeWhile(_.nonEmpty)
      .foreach(line => println(process(line)))


  //    .foreach { x =>
  //      x map process foreach println
  //    }
  //  process("texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc") foreach println
}
}
