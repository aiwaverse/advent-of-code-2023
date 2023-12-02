import scala.io.Source
import scala.math
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import scala.util.matching.Regex

@main def run(path: String): Unit =
  val digits = Range.inclusive(1, 9).map(_.toString) ++ Iterable(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )
  val digitsSum = Try {
    Source
      .fromFile(path)
      .getLines()
      .map(line => {
        val matches = digits.filter(line.contains(_))
        val first = matches.minBy(line.indexOf(_))
        val last = matches.maxBy(line.lastIndexOf(_))
        val sum =
          (if first.length == 1 then first else convertToNumberString(first))
            +
              (if last.length == 1 then last else convertToNumberString(last))
        println(s"$line => $first + $last => $sum")
        sum.toInt
      })
      .reduce(_ + _)
  }

  digitsSum match {
    case Success(value)     => println(s"The total sum was $value")
    case Failure(exception) => println(s"There was an exception\n$exception")
  }

def convertToNumberString(n: String): String =
  n match {
    case "one"   => "1"
    case "two"   => "2"
    case "three" => "3"
    case "four"  => "4"
    case "five"  => "5"
    case "six"   => "6"
    case "seven" => "7"
    case "eight" => "8"
    case "nine"  => "9"
    case s       => s
  }
