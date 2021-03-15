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
  abstract class Person(name: String, drink: String, age: Int)

  case class Adult(name: String, drink: String, age: Int) extends Person(name, drink, age)
  case class Minor(name: String, drink: String, age: Int, parentName: String) extends Person(name, drink, age)

  val file3 = Source
    .fromFile("partycipants2.txt")
    .getLines()
    .toList

  val result = new ListBuffer[Person]

  def isNumber(line: String): Boolean = line.matches("[0-9]+")

  for (line <- 2 to file3.length -1) {
    if (isNumber(file3(line))) {
      file3(line).toInt match {
        case x if (x < 18)  => {
          result += Minor(file3(line - 2), file3(line - 1), file3(line).toInt, file3(line + 1))
        }
        case x if (x >= 18) => {
          result += Adult(file3(line - 2), file3(line - 1), file3(line).toInt)
        }
      }
    }
  }

  result.toList
  result.foreach(println)
}
