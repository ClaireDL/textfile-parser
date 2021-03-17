package com.clairedl.scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App {
  //
  // Exercise 1: load a textfile that has a known structure (1 element has 3 lines) and print it
  //

  // case class Ingredient(name: String, calories: Int, weight: Double) {
  //   override def toString(): String = s"Ingredient: $name, Cal./100g: $calories, Quant. in g: $weight"
  // }

  // val file = Source
  //   .fromFile("ragu.txt")
  //   .getLines()
  //   .toList
  //   .grouped(3)
  //   .map(x => Ingredient(x(0), x(1).toInt, x(2).toDouble))
  //   .toList

  // file.foreach(println)

  case class Participant(name: String, drink: String, age: Int)

  val file2 = Source
    .fromFile("partycipants.txt")
    .getLines()
    .toList
    .grouped(3)
    .map(x => Participant(x(0), x(1), x(2).toInt))
    .toList

  // file2.foreach(println)

  //
  // Exercise 2: structure is variable: under 18 years old have a 4th line, parent name
  //

  case class Person(name: String, drink: String, age: Int, parentName: String)

  object Person {
    def apply(name: String, drink: String, age: Int): Person = Person(name, drink, age, "")
  }

  // val file3 = Source
  //   .fromFile("partycipants2.txt")
  //   .getLines()
  //   .toList

  // val result = new ListBuffer[Person]

  // def isNumber(line: String): Boolean = line.matches("[0-9]+")

  // for (line <- 2 to file3.length -1) {
  //   if (isNumber(file3(line))) {
  //     file3(line).toInt match {
  //       case x if (x < 18)  =>
  //         result += Person(file3(line - 2), file3(line - 1), file3(line).toInt, file3(line + 1))

  //       case x if (x >= 18) =>
  //         result += Person(file3(line - 2), file3(line - 1), file3(line).toInt)
  //     }
  //   }
  // }

  // val parsed = result.toList
  // parsed.map(x => println(x.name))
  // parsed.foreach(println)

  //
  // Exercise 3: load one line at a time
  //

  val result2 = new ListBuffer[Person].empty
  var buffer = new ListBuffer[String].empty

  for (line <- Source.fromFile("partycipants2.txt").getLines()) {
    buffer += line
    if (buffer.length == 4) {
      if (buffer(2).toInt < 18) {
        result2 += Person(buffer(0), buffer(1), buffer(2).toInt, buffer(3))
        buffer = ListBuffer()
      }
      else {
        result2 += Person(buffer(0), buffer(1), buffer(2).toInt)
        buffer = ListBuffer(buffer(3))
      }
    }
  }
  result2.toList
  result2.foreach(println)
}
