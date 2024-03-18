package u03

object Persons:

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n
    def getCourseName(p: Person): String = p match
      case Teacher(_, c) => c
      case _ => "nil"
    // a method outside the Person module
    def isStudent(p: Person): Boolean = p match
      case Student(_, _) => true
      case _ => false
@main def tryPersons =
  import Persons.*
  import Person.*