package com.clairedl.scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App {
  case class Ingredient(name: String, calories: Int, weight: Double) {
    override def toString(): String = s"Ingredient: $name, Calories/100g: $calories, Quantity used: $weight"
  }

  abstract class Converter[A] {
    def convert(elements: List[String]): A
  }

  def loadAndConvert[A](filepath: String, converter: Converter[A], parameterNumber: Int): List[A] = {
    val file = Source
      .fromFile(filepath)
      .getLines()
      .toList
      .grouped(parameterNumber)
      .toList

    file.map(x => converter.convert(x))
  }

  class ConvertIngredients extends Converter[Ingredient] {
    def convert(group: List[String]): Ingredient = Ingredient(group(0), group(1).toInt, group(2).toDouble)
  }

  val converter = new ConvertIngredients
  val result = loadAndConvert("ragu.txt", converter, 3)
  result.foreach(println)
}
