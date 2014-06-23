package challenge168

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try

/**
 * Solution to challenge #168 on Reddit /r/dailyprogrammer
 * http://www.reddit.com/r/dailyprogrammer/comments/28vgej/6232014_challenge_168_easy_final_grades_test_data/
 * @author Jacob Taylor-Hindle
 */
object Solution168 {
  // Represents a student's grades.
  type Grades = List[Int]

  // Represents multiple unique student records.
  type Records = Map[Student, Grades]

  // Files containing first & last names.
  lazy val firstNames = Source.fromFile("src/main/scala/challenge168/firstnames.txt").getLines().toArray
  lazy val lastNames = Source.fromFile("src/main/scala/challenge168/lastnames.txt").getLines().toArray

  // A set containing all of the students generated thus far.
  var generatedStudents = Set[Student]()

  // Max number of grades.
  val NUM_OF_GRADES = 5

  // Max student score.
  val MAX_SCORE = 101

  // Random number generator.
  val rand = Random

  def main(args: Array[String]): Unit = {
    if (args.isEmpty || args.length > 1) {
      println("Please provide a single number specifying the number of records to generate.")
    } else {
      Try {
        Integer.parseInt(args(0))
      } match {
        case Success(n) => generateRecords(n).foreach(t => printRecord(t._1, t._2))
        case Failure(_) => println("Please only enter integers.")
      }
    }
  }

  /**
   * Generates random student records.
   * @param n
   *     The number of student records to generate.
   * @return
   *     n student records.
   */
  private def generateRecords(n: Int): Records = {
    (1 to n).map {
      _ => generateStudent -> generateGrades
    }.toMap
  }

  /**
   * Generates a random student.
   * @return
   *     a random student.
   */
  @tailrec
  private def generateStudent: Student = {
    // Select a random first & name
    val firstIndex = rand.nextInt(firstNames.length)
    val lastIndex = rand.nextInt(lastNames.length)

    val stud = Student(firstNames(firstIndex), lastNames(lastIndex))

    // If we have already generated the student, generate
    // another instead to avoid duplicates.
    if (generatedStudents.contains(stud)) {
      generateStudent
    } else {
      generatedStudents += stud
      stud
    }
  }

  /**
   * Generates NUM_OF_GRADES random grades.
   * @return
   *     A list of integers from 0 to MAX_SCORE.
   */
  private def generateGrades: Grades = {
    List.fill(NUM_OF_GRADES) {
      rand.nextInt(MAX_SCORE)
    }
  }

  /**
   * Prints the given record to the
   * standard output.
   * @param t
   *     A tuple containing a student along with
   *     their corresponding grades.
   */
  private def printRecord(t: (Student, Grades)) = {
    //Extract student & grade from tuple.
    val (s, g) = t

    println(s"${s.firstName}, ${s.lastName}, ${g.mkString(", ")}")
  }
}

/**
 * Represents a student.
 */
case class Student(firstName: String, lastName: String)

