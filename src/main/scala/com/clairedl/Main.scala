package com.clairedl.scala

import scala.io.Source
import scala.annotation.tailrec
import java.rmi.server.Operation

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

  // val file2 = Source
  //   .fromFile("partycipants.txt")
  //   .getLines()
  //   .toList
  //   .grouped(3)
  //   .map(x => Participant(x(0), x(1), x(2).toInt))
  //   .toList

  // file2.foreach(println)

  //
  // Exercise 2: structure is variable: under 18 years old have a 4th line, parent name
  //

  abstract class Person {
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

  // def process(fileName: String): List[Person] = {
    // val source = Source.fromFile(fileName).getLines().toList
    // var result = List[Person]()


    // for (line <- 2 to source.length - 1) {
    //   source(line).toIntOption match {
    //     case Some(x) => {
    //       if (source(line).toInt < 18) result = Minor(source(line - 2), source(line - 1), source(line).toInt, source(line + 1)) :: result

    //       else if (source(line).toInt >= 18) result = Adult(source(line - 2), source(line - 1), source(line).toInt) :: result
    //     }
    //     case None =>
    //   }
    // }
    // result
  // }

  // val participants = process("partycipants2.txt")

  // println(s"This is the final list")
  // participants.map(x => println(x))

  val source = Source.fromFile("Partycipants2.txt").getLines().toList

  @tailrec
  def countLinesRec(input: List[String], counter: Int): Int =
    input match {
      case Nil        => counter
      case _ :: tail  => countLinesRec(input.drop(1), counter + 1)
    }

  val result = countLinesRec(source, 0)
  // println(result)

  @tailrec
  def checkIntRec(input: List[String], output: List[Int]): List[Int] = {
    input match {
      case Nil       => output
      case _ :: tail => input(0).toIntOption match {
        case Some(x) => checkIntRec(input.drop(1), input(0).toInt :: output)
        case None    => checkIntRec(input.drop(1), output)
      }
    }
  }
  val resultCheckInt = checkIntRec(source, List[Int]())
  // println(resultCheckInt)

  @tailrec
  def checkAgeRec(input: List[String], buffer: List[String], output: List[Person]): List[Person] = {
    if (input.isEmpty) output
    else {
      if (buffer.isEmpty) checkAgeRec(input.drop(1), input(0) :: Nil, output)
      else buffer(0).toIntOption match {
        case Some(x) => {
          val age = buffer(0).toInt
          val name = buffer(2)
          val drink = buffer(1)
          // input(0) is the name of the next person if age > 18, name of the person's parent if age < 18

          age match {
            case x if (x >= 18) => checkAgeRec(input.drop(1), List(input(0)), Adult(name, drink, age) :: output)
            case x if (x < 18)  => checkAgeRec(input.drop(1), List[String](), Minor(name, drink, age, input(0)) :: output)
          }
        }
        case None    => checkAgeRec(input.drop(1), input(0) :: buffer, output)
      }
    }
  }

  val resultCheckAge = checkAgeRec(source, List[String](), List[Person]())
  println(resultCheckAge)

  // Working on foldLeft
  @tailrec
  val priceList = List(15.99, 12.00, 20)

  def sumAndConvert(input: List[Double], conversion: Double, accumulator: Double): Double = {
    if (input.isEmpty) accumulator
    else {
      val sum = accumulator + input(0) * conversion
      sumAndConvert(input.drop(1), conversion, sum)
    }
  }

  val total = sumAndConvert(priceList, 0.8698, 0)
  println(s"total with function: $total")

  val totalWithBuiltIn = priceList.sum * 0.8698
  println(s"total with built-in: $totalWithBuiltIn")

  val totalFoldLeft = priceList.foldLeft(0.0)(_ + _ * 0.8698)
  println(s"total with foldLeft: $totalFoldLeft")

  // Generalising the transformation

  // Using Strategy pattern
  abstract class Transformation[A, B](currentValue: A, accumulator: B) {
    def transform(): B
  }

  class SumAndConvert(currentValue: Double, accumulator: Double, exchangeRate: Double) extends Transformation(currentValue, accumulator) {
    def transform(): Double = accumulator + currentValue * exchangeRate
  }

  def listTransformationStrategy[A, B](input: List[A], transformation: Transformation[A, B], accumulator: B): B = {
    if (input.isEmpty) accumulator
    else {
      listTransformationStrategy(input.drop(1), transformation, transformation.transform())
    }
  }

  // Using functions

  // I need a Transformation with this A and this B
  def listTransformationFunc[A, B](input: List[A], transformation: (A, B) => B, accumulator: B): B = {
    if (input.isEmpty) accumulator
    else {
      val updateAcc: B = transformation(input(0), accumulator)
      listTransformationFunc(input.drop(1), transformation, updateAcc)
    }
  }

  val sumAndConvertFunc = (currentValue: Double, exchangeRate: Double) => currentValue * exchangeRate

  // val updateWalletFunc = listTransformationFunc(priceList, sumAndConvertFunc(priceList(0), 0.8698), 0)
  // println(updateWalletFunc)

  val updateWalletStra = listTransformationStrategy(priceList, new SumAndConvert(priceList(0), 0, 0.8698), 0)
}
