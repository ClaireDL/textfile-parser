package com.clairedl.scala

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

  trait Person {
    def name: String
    def drink: String
    def age: Int
  }
  case class Adult(name: String, drink: String, age: Int) extends Person
  case class Minor(name: String, drink: String, age: Int, parentName: String) extends Person

  // val file3 = Source
  //   .fromFile("partycipants2.txt")
  //   .getLines()
  //   .toList

  // var result = List[Person]()

  // def isNumber(line: String): Boolean = line.matches("[0-9]+")

  // for (line <- 2 to file3.length -1) {
  //   if (isNumber(file3(line))) {
  //     file3(line).toInt match {
  //       case x if (x < 18)  =>
  //         result = Minor(file3(line - 2), file3(line - 1), file3(line).toInt, file3(line + 1)) :: result

  //       case x if (x >= 18) =>
  //         result = Adult(file3(line - 2), file3(line - 1), file3(line).toInt) :: result
  //     }
  //   }
  // }

  // result.map(x => println(x.name))
  // result.foreach(println)

  //
  // Exercise 3: load one line at a time
  //

  def process(fileName: String): List[Person] = {
    val source = Source.fromFile(fileName).getLines().toList
    var result = List[Person]()

    for (line <- 2 to source.length - 1) {
      source(line).toIntOption match {
        case Some(x) => {
          if (source(line).toInt < 18) result = Minor(source(line - 2), source(line - 1), source(line).toInt, source(line + 1)) :: result

          else if (source(line).toInt >= 18) result = Adult(source(line - 2), source(line - 1), source(line).toInt) :: result
        }
        case None =>
      }
    }
    result
  }

  val participants = process("partycipants2.txt")

  println(s"This is the final list")
  participants.map(x => println(x))
}
