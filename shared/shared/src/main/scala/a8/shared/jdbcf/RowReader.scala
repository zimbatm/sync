package a8.shared.jdbcf


import java.time.{LocalDateTime, LocalTime}
import a8.shared.json.ast.{JsObj, JsVal}

import scala.reflect.ClassTag
import a8.shared.SharedImports._

import java.io.BufferedReader

object RowReader extends MoreRowReaderCodecs with RowReaderTuples {

  trait TupleReader[A] extends RowReader[A] {
    class UnsafeReader(startIndex: Int, row: Row) {
      var offset = 0
      def next[B : RowReader]: B = {
        val t = RowReader[B].rawRead(row, startIndex+offset)
        offset += t._2
        t._1
      }
    }
  }

  def apply[A : RowReader]: RowReader[A] = implicitly[RowReader[A]]

  def singleColumnReader[A : ClassTag](fn: PartialFunction[AnyRef,A]): RowReader[A] =
    new RowReader[A] {
      override def rawRead(row: Row, index: Int): (A, Int) = {
        row.coerceByIndex[A](index)(fn) -> 1
      }
    }

  implicit lazy val intReader: RowReader[Int] = singleColumnReader[Int] {
    case i: java.lang.Number => i.intValue()
  }
  implicit lazy val stringReader: RowReader[String] = singleColumnReader[String] {
    case s: String =>
      s
    case clob: java.sql.Clob =>
      clob.getCharacterStream.readFully()
  }

  implicit lazy val localDateTimeMapper: RowReader[LocalDateTime] =
    singleColumnReader[LocalDateTime] {
      case ts: java.sql.Timestamp =>
        ts.toLocalDateTime
    }

  implicit lazy val localTimeMapper: RowReader[LocalTime] =
    singleColumnReader[LocalTime] {
      case ts: java.sql.Time =>
        ts.toLocalTime
    }

  implicit lazy val byteReader: RowReader[Byte] =
    singleColumnReader[Byte] {
      case i: java.lang.Number =>
        i.byteValue()
    }

  implicit lazy val shortReader: RowReader[Short] =
    singleColumnReader[Short] {
      case i: java.lang.Number =>
        i.shortValue()
    }

  implicit lazy val longReader: RowReader[Long] =
    singleColumnReader[Long] {
      case i: java.lang.Number =>
        i.longValue()
    }

  implicit lazy val floatReader: RowReader[Float] =
    singleColumnReader[Float] {
      case i: java.lang.Number =>
        i.floatValue()
    }

  implicit lazy val doubleReader: RowReader[Double] =
    singleColumnReader[Double] {
      case i: java.lang.Number =>
        i.doubleValue()
    }

  implicit lazy val boolean: RowReader[Boolean] =
    singleColumnReader[Boolean] {
      case s: String =>
        s.toLowerCase match {
          case "y" | "true" | "yes" | "1" =>
            true
          case _ =>
            false
        }
      case b: java.lang.Boolean =>
        b

    }

  implicit def JsObjReader: RowReader[JsObj] =
    (row: Row, _: Int) => row.unsafeAsJsObj -> row.unsafeAsJsObj.size

  implicit def JsValReader: RowReader[JsVal] =
    singleColumnReader[JsVal] {
      case v =>
        unsafe.coerceToJsVal(v)
    }

  implicit def optionReader[A : RowReader]: RowReader[Option[A]] =
    new RowReader[Option[A]] {
      val rowReaderA = implicitly[RowReader[A]]
      override def rawRead(row: Row, index: Int): (Option[A], Int) = {
        row.rawValueByIndex(index) match {
          case None =>
            None -> 1
          case _ =>
            val t = rowReaderA.rawRead(row, index)
            Some(t._1) -> t._2
        }
      }
    }

}

trait RowReader[A] { outer =>

  def read(row: Row): A = read(row, 0)
  final def read(row: Row, index: Int): A = rawRead(row, index)._1

  def rawRead(row: Row, index: Int): (A,Int)

  def readOpt(row: Row, index: Int): Option[A] =
    row.rawValueByIndex(index) match {
      case None =>
        None
      case null =>
        None
      case _ =>
        Some(read(row, index))
    }

  def map[B](fn: A=>B): RowReader[B] =
    new RowReader[B] {
      override def rawRead(row: Row, index: Int): (B, Int) = {
        val t = outer.rawRead(row, index)
        fn(t._1) -> t._2
      }
    }

}
