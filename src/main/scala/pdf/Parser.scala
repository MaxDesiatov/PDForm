package pdf

import util.parsing.combinator._
import util.parsing.combinator.syntactical._
import util.parsing.input.{ Position, Reader }
import util.parsing.input.CharArrayReader.EofCh
import annotation.tailrec

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

// FIXME: this should be done as an extended string class with some unary operator
object Line {
    def apply(s: Any) = s + Nl()
}

object Nl {
    def apply() = "\n"
}

object Nl2 {
    def apply() = Nl().length match {
        case 1 => " " + Nl()
        case 2 => Nl()
        case _ => " \n"
    }
}

class Colour

class RGBColour(r: Int, g: Int, b: Int) extends Colour

class CMYKColour(c: Int, m: Int, y: Int, k: Int) extends Colour

class Dot(x: Int, y: Int, colour: Colour)

sealed abstract class PdfElement

case class PdfObject() extends PdfElement

case class PdfComment(value: String) extends PdfObject {
    override def toString = Line("%" + value)
}

case class PdfBoolean(value: Boolean) extends PdfObject {
    override def toString = Line(value)
}

case class PdfNumeric(value: Double) extends PdfObject {
    override def toString = Line(value)
}

case class PdfString(value: Seq[Byte]) extends PdfObject {
    override def toString = Line("(" + value + ")")
}

case class PdfName(value: String) extends PdfObject {
    override def toString = Line("/" + value)
}

case class PdfArray(value: Seq[PdfObject]) extends PdfObject {
    override def toString = Line(value mkString ("[", " ", "]"))
}

case class PdfDictionary(value: Map[PdfName, PdfObject]) extends PdfObject {
    override def toString = Line((for ((x, y) <- value)
        yield x + " " + y) mkString ("<< ", Nl(), " >>"))
}

case class PdfStream(dictionary: PdfDictionary, stream: Seq[Byte]) extends PdfObject {
    override def toString = Line(dictionary + Line("stream") + Line(stream) + Line("endstream"))
}

case class PdfNull extends PdfObject {
    override def toString = Line("null")
}

case class PdfIndirect(number: Int, generation: Int, obj: PdfObject) extends PdfObject {
    override def toString = Line(number + " " + " " + generation + "obj") + obj + Line("endobj")
}

case class PdfXrefSection(subsections: Seq[PdfXrefSubsection]) extends PdfElement {
    override def toString = Line("xref") + subsections.mkString
}

case class PdfXrefSubsection(firstEntry: Int,
                             nOfEntries: Int,
                             entries: Seq[PdfXrefSubsectionEntry]) extends PdfElement {
    override def toString = Line(firstEntry + " " + nOfEntries) + entries.mkString
}

case class PdfXrefSubsectionEntry(number: Int, generation: Int, inUse: Boolean) extends PdfElement {
    override def toString = Line(format("%010d", number) + " " +
        format("%05d", generation) + " " + (if (inUse) "n" else "f") + Nl2())
}

case class PdfTrailer(dictionary: PdfDictionary, startxref: Int) extends PdfElement {
    override def toString = Line("trailer") + dictionary + 
    	Line("startxref") + Line(startxref) + PdfComment("%EOF")
}

//object PdfParser extends StandardTokenParsers {
//  lexical.delimiters ++= List("+")
//
//  def comment = "%" ~> stringLit ^^ { s => PdfComment(s) }
//
//  def sum = value ~ "+" ~ value ^^ {
//    case left ~ "+" ~ right =>
//      EAdd(left, right)
//  }
//
//  def expr = (sum | value) //top level expression
//
//  def parse(b: Seq[Byte]) = {
//    val tokens = new lexical.Scanner(b)
//    phrase(expr)(tokens)
//  }
//
//  def apply(s: Seq[Byte]): Seq[PdfElement] = {
//    parse(s) match {
//      case Success(tree, _) => tree
//      case e: NoSuccess =>
//        throw new IllegalArgumentException("Bad syntax: " + s)
//    }
//  }
//}

object Parser {
    def main(args: Array[String]): Unit = {
        val v = true
    }
}

trait ParsersUtil extends Parsers {
    lazy val anyElem: Parser[Elem] = elem("anyElem", _ => true)
    def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => !(xs contains x))
    def elemOf(xs: Elem*): Parser[Elem] = elem("elemOf", xs contains _)

    def take(n: Int): Parser[Seq[Elem]] = repN(n, anyElem)
    def takeUntil(cond: Parser[Elem]): Parser[Seq[Elem]] = takeUntil(cond, anyElem)
    def takeUntil(cond: Parser[Elem], p: Parser[Elem]): Parser[Seq[Elem]] = rep(not(cond) ~> p)
    def takeWhile(p: Parser[Elem]): Parser[Seq[Elem]] = rep(p)
}

case class ByteOffsetPosition(offset: Int) extends Position {
    final val line = 1
    def column = offset + 1
    def lineContents: String = ""
}

class ByteReader(val bytes: Array[Byte], override val offset: Int) extends Reader[Byte] {
    def this(reader: Reader[_]) = this(reader.source.toString.getBytes, 0)
    def this(bytes: Seq[Byte]) = this(bytes.toArray, 0)
    def this(str: String) = this(str.getBytes, 0)

    override def source = bytes map (_.toChar)

    def first: Byte = if (offset < bytes.length) bytes(offset) else EofCh.toByte
    def rest: ByteReader = if (offset < bytes.length) new ByteReader(bytes, offset + 1) else this
    def pos: Position = ByteOffsetPosition(offset)
    def atEnd = offset >= bytes.length

    def byteAt(n: Int) = bytes(n)
    def length = bytes.length - offset

    override def drop(n: Int): ByteReader = new ByteReader(bytes, offset + n)
    def take(n: Int): Seq[Byte] = bytes drop offset take n

    override def toString = "ByteReader(%d / %d)".format(offset, bytes.length)
}

trait BinaryParsers extends Parsers with ParsersUtil {
    type Elem = Byte

    protected implicit def readerToByteReader(x: Input): ByteReader = x match {
        case br: ByteReader => br
        case _ => new ByteReader(x)
    }
    def toInt(bytes: Seq[Byte]): Int = bytes.foldLeft(0)((x, b) => (x << 8) + (b & 0xFF))
    def toLong(bytes: Seq[Byte]): Long = bytes.foldLeft(0L)((x, b) => (x << 8) + (b & 0xFF))

    lazy val byte: Parser[Byte] = anyElem
    lazy val u1: Parser[Int] = byte ^^ (_ & 0xFF)
    lazy val u2: Parser[Int] = bytes(2) ^^ toInt
    lazy val u4: Parser[Int] = bytes(4) ^^ toInt
    lazy val u4f: Parser[Float] = u4 ^^ intBitsToFloat
    lazy val u8: Parser[Long] = bytes(8) ^^ toLong
    lazy val u8d: Parser[Double] = u8 ^^ longBitsToDouble

    def bytes(n: Int): Parser[Seq[Byte]] = Parser { in =>
        if (n <= in.length) Success(in take n, in drop n)
        else Failure("Requested %d bytes but only %d remain".format(n, in.length), in)
    }

    def parse[T](p: Parser[T], in: Input): ParseResult[T] = p(in)
    def parse[T](p: Parser[T], in: String): ParseResult[T] = parse(p, new ByteReader(in))
}
