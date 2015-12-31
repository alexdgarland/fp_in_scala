package fp_in_scala.chapter_4


case class Person(name : Name, age : Age)

case class Name(value : String)

case class Age(value : Int)


object Person {

  def mkName(name : String) : MyEither[String, Name] = {
    if (name == "" || name == null) MyLeft("Name is empty.")
    else MyRight(Name(name))
  }

  def mkAge(age : Int) : MyEither[String, Age] = {
    if (age < 0) MyLeft("Age is out of range.")
    else MyRight(Age(age))
  }

  def mkPerson(name : String, age : Int) : MyEither[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }

}
