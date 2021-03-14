package com.clairedl.scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App {
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

  case class Ingredient(name: String, calories: Int, weight: Double) {
    override def toString(): String = s"Ingredient: $name, Cal./100g: $calories, Quant. in g: $weight"
  }

  class ConverterIngredients extends Converter[Ingredient] {
    def convert(content: List[String]): Ingredient = Ingredient(content(0), content(1).toInt, content(2).toDouble)
  }

  val converter = new ConverterIngredients
  val result = loadAndConvert("ragu.txt", converter, 3)
  result.foreach(println)

  case class Partygoer(name: String, drink: String, age: Int)

  class ConverterParticipants extends Converter[Partygoer] {
    def convert(details: List[String]): Partygoer = Partygoer(details(0), details(1), details(2).toInt)
  }

  val converterParty = new ConverterParticipants
  val guestList = loadAndConvert("partycipants.txt", converterParty, 3)
  guestList.foreach(println)
}
