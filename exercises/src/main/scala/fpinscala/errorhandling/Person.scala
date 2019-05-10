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
}