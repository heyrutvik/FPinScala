package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] = 
    mkName(name).map2(mkAge(age))(Person(_, _))

  def mkName1(name: String): Check[String, Name] = {
    if (name == "" || name == null) Fail(List("Name is empty"))
    else Pass(new Name(name))
  }

  def mkAge1(age: Int): Check[String, Age] = {
    if (age < 0) Fail(List("Age is out of range"))
    else Pass(new Age(age))
  }

  def mkPerson1(name: String, age: Int): Check[String, Person] = 
    mkName1(name).map2(mkAge1(age))(Person(_, _))
}