package com.clairedl.scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App {
  case class Ingredient(name: String, calories: Int, weight: Double) {
    override def toString(): String = s"Ingredient: $name, Cal./100g: $calories, Quant. in g: $weight"
  }

  val file = Source
    .fromFile("ragu.txt")
    .getLines()
    .toList
    .grouped(3)
    .map(x => Ingredient(x(0), x(1).toInt, x(2).toDouble))
    .toList

  file.foreach(println)

  case class Person(name: String, drink: String, age: Int)

  val file2 = Source
    .fromFile("partycipants.txt")
    .getLines()
    .toList
    .grouped(3)
    .map(x => Person(x(0), x(1), x(2).toInt))
    .toList

  file2.foreach(println)
}
