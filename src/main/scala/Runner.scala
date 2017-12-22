import java.time.LocalDateTime

import atto.Parser
import shapeless._

import scala.reflect.ClassTag

object Runner extends App {

  import atto.Atto._

  case class Person(name: String, age: Int, lastLogin: Option[LocalDateTime])

  implicit val stringParser: Parser[String] = stringOf(letterOrDigit)
  implicit val intParser: Parser[Int] = int
  implicit val doubleParser: Parser[Double] = double
  implicit val longParser: Parser[Long] = long
  implicit val dateTimeParser: Parser[LocalDateTime] = orElse(string("null").map(_ => null), stringOf(letterOrDigit | oneOf("-:.")).map(LocalDateTime.parse))

  implicit def optionA[A](implicit parser: Parser[A]): Parser[Option[A]] = orElse(string("None").map(_ => Option.empty), string("Some") ~> parens(parser.map(Option.apply)))

  implicit def deriveHNil: Parser[HNil] = endOfChunk.map(_ => HNil)

  implicit def deriveHCons[V, T <: HList](implicit hParser: Lazy[Parser[V]],
                                          tParser: Lazy[Parser[T]]
                                         ): Parser[V :: T] = {
    for {
      head <- hParser.value <~ orElse(char(','), char(')'))
      tail <- tParser.value
    } yield head :: tail
  }

  implicit def deriveClass[A, R](implicit generic: Generic.Aux[A, R],
                                 parser: Lazy[Parser[R]],
                                 classTag: ClassTag[A]): Parser[A] = {
    val classNameParser = string(classTag.runtimeClass.getSimpleName + "(")
    for {
      _ <- classNameParser
      hlist <- parser.value
    } yield generic.from(hlist)
  }

  val personString = "Person(John,30,Some(2017-12-22T10:41:23.064))"

  println(implicitly[Parser[Person]].parse(personString))

}
