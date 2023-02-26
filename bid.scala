import scala.io.Source
import collection.mutable

object Main {
  case class Card(v: Int) extends AnyVal

  case class CardValue(v: Int) extends AnyVal {
    def of(s: Suite): Card = Card(v << s.v)
  }
  val cA  = CardValue(0x80)
  val c10 = CardValue(0x40)
  val cK  = CardValue(0x20)
  val cQ  = CardValue(0x10)
  val cJ  = CardValue(0x08)
  val c9  = CardValue(0x04)
  val c8  = CardValue(0x02)
  val c7  = CardValue(0x01)

  case class Suite(v: Int) extends AnyVal
  val notrump  = Suite(26)
  val alltrump = Suite(25)
  val spade    = Suite(24)
  val diamond  = Suite(16)
  val heart    = Suite(8)
  val club     = Suite(0)

  case class Hand(v: Int) extends AnyVal {
    def openInFour: Boolean = ???
    def jn3rd: Boolean = ???
    def suites: Int = ???

    def aces: Int =
      (if (contains(cA of spade)) 1 else 0) +
      (if (contains(cA of diamond)) 1 else 0) +
      (if (contains(cA of heart)) 1 else 0) +
      (if (contains(cA of club)) 1 else 0)

    def jacks: Int =
      (if (contains(cJ of spade)) 1 else 0) +
      (if (contains(cJ of diamond)) 1 else 0) +
      (if (contains(cJ of heart)) 1 else 0) +
      (if (contains(cJ of club)) 1 else 0)

    def contains(card: Card): Boolean =
      (v & card.v) != 0
  }

  sealed trait RawBid
  case class ValueBid(n: Int, s: String) extends RawBid
  case object Passs extends RawBid
  case object Doubles extends RawBid
  case object Redoubles extends RawBid

  object Bid extends Enumeration {
    type Bid = Value
    val P10Rz, P10Sw, P10SA, P10TA,
        P20Rz, P20Sw, P20SA, P20TA,
        P30Rz, P30Sw, P30SA, P30TA,
        P40Rz, P40Sw, P40SA, P40TA,
        P50Rz, P50Sw, P50SA, P50TA,
        P60Rz, P60Sw, P60SA, P60TA,
        P70Rz, P70Sw, P70SA, P70TA,
        Pass = Value
  }
  import Bid._

  case class System(bids: (Bid, Hand => Boolean, System)*)

  // As player 1
  val system = System(
     (Pass, _.aces < 2, null),
     (P10Sw, _.aces >= 2, null),
     (P10SA, _.aces >= 3, null),
     (P10TA, x => x.aces >= 1 && x.jacks >= 2, null),
     (P20Sw, _ => true, null),
     (P20SA, _.aces == 2, null),
     (P20TA, _.jacks >= 3, null),
     (P30Sw, x => x.suites == 4 && x.jn3rd, null),
     (P30SA, _.aces == 4, null),
     (P30TA, _.jacks == 4, null),
     (P40Sw, x => x.suites == 3 && x.jn3rd, null),
     (P40SA, null, null),
     (P40TA, null, null),
     (P50Sw, x => x.jn3rd /*&& x.contains(Sw+1)*/, null),
     (P50SA, null, null),
     (P50TA, null, null),
     (P60Sw, x => x.jn3rd /*&& x.contains(Sw+2)*/, null),
     (P60SA, null, null),
     (P60TA, null, null),
     (P70Sw, x => x.jn3rd /*&& x.contains(Sw+3)*/, null),
     (P70SA, null, null),
     (P70TA, null, null)
  )

  def check(h1: Hand, h2: Hand, bids: List[Bid]): Boolean = {
    def check0(h1: Hand, h2: Hand, bids: List[Bid], system: System): Boolean = {
      bids match {
        case Pass :: Pass :: Pass :: Pass =>
          true
        case b :: _ =>
          val (bid, pred, subsystem) = system.bids.find(_._1 == b).get
          pred(h1)
          // check0(h2, h1, bs, subsystem)
        case _ =>
          false
      }
    }

    check0(h1, h2, bids, system)
  }

  def main(args: Array[String]): Unit = {
    args.foreach { path =>
      val round = Round.read(path)
      val normalized = normalize(round.bids)
      println(normalized)
      if (!check(round.h1, round.h3, normalized))
        println(round)
    }
  }

  // val P10Rz, P10Sw, P10SA, P10TA,
  //     P20Rz, P20Sw, P20SA, P20TA,
  //     P30Rz, P30Sw, P30SA, P30TA,
  //     P40Rz, P40Sw, P40SA, P40TA,
  //     P50Rz, P50Sw, P50SA, P50TA,
  //     P60Rz, P60Sw, P60SA, P60TA,
  //     P70Rz, P70Sw, P70SA, P70TA,
  //     Pass = Value
  def normaliz(bids: List[RawBid]): List[Bid] = Nil
  def normalize(bids: List[RawBid]): List[Bid] = {
    def Sw(n: Int) = n match {
      case 82 => P10Sw
      case 90 => P20Sw
      case 100 => P30Sw
      case 110 => P40Sw
      case 120 => P50Sw
      case 130 => P60Sw
    }
    def TA(n: Int) = n match {
      case 82 => P10TA
      case 90 => P20TA
      case 100 => P30TA
      case 110 => P40TA
      case 120 => P50TA
      case 130 => P60TA
    }
    def SA(n: Int) = n match {
      case 82 => P10SA
      case 90 => P20SA
      case 100 => P30SA
      case 110 => P40SA
      case 120 => P50SA
      case 130 => P60SA
    }
    bids match {
      case Passs :: Passs :: Passs :: Passs :: _ => Nil
      case Passs :: xs => Pass :: normaliz(xs)
      case ValueBid(n, "♠") :: xs => Sw(n) :: normaliz(xs)
      case ValueBid(n, "♥") :: xs => Sw(n) :: normaliz(xs)
      case ValueBid(n, "♣") :: xs => Sw(n) :: normaliz(xs)
      case ValueBid(n, "♦") :: xs => Sw(n) :: normaliz(xs)
      case ValueBid(n, "TA") :: xs => TA(n) :: normaliz(xs)
      case ValueBid(n, "SA") :: xs => SA(n) :: normaliz(xs)
      case Doubles :: _ => Nil
      case Redoubles :: _ => Nil
      case _ => ???
    }
  }

  case class Round(
    h1: Hand,
    h2: Hand,
    h3: Hand,
    h4: Hand,
    bids: List[RawBid],
    path: String
  ) {
    override def toString: String = {
      def show(x: Hand): List[String] = {
        def shov(suite: Suite): String = {
          val sb = new StringBuilder()
          if (x.contains(cA of suite)) sb.append("A ")
          if (x.contains(c10 of suite)) sb.append("10 ")
          if (x.contains(cK of suite)) sb.append("K ")
          if (x.contains(cQ of suite)) sb.append("Q ")
          if (x.contains(cJ of suite)) sb.append("J ")
          if (x.contains(c9 of suite)) sb.append("9 ")
          if (x.contains(c8 of suite)) sb.append("8 ")
          if (x.contains(c7 of suite)) sb.append("7 ")
          sb.toString
        }
        val s = shov(spade)
        val d = shov(diamond)
        val h = shov(heart)
        val c = shov(club)
        val m = s.size max d.size max h.size max c.size
        "╔══" + "".padTo(m, "═").mkString + "═╗" ::
        "║ ♠ " + s.padTo(m, " ").mkString + "║" ::
        "║ ♥ " + h.padTo(m, " ").mkString + "║" ::
        "║ ♣ " + c.padTo(m, " ").mkString + "║" ::
        "║ ♦ " + d.padTo(m, " ").mkString + "║" ::
        "╚══" + "".padTo(m, "═").mkString + "═╝" :: Nil
      }

      var bidLog = "╔════════════════╗" :: s"$path:" :: Nil
      bids.zipWithIndex.map {
        case (x, i) => (x, "P" + (i % 4 + 1))
      }.map {
        case (Passs, p) => s"║ $p passes"
        case (Doubles, p) => s"║ $p doubles"
        case (Redoubles, p) => s"║ $p redoubles"
        case (ValueBid(n, s), p) => s"║ $p bids $n $s"
      }.foreach(s => bidLog ::= s.padTo(17, " ").mkString + "║")
      bidLog ::= "╚════════════════╝"
      bidLog = bidLog.reverse

      val s1 = mutable.Stack.from(show(h1))
      val s2 = mutable.Stack.from(show(h2))
      val s3 = mutable.Stack.from(show(h3))
      val s4 = mutable.Stack.from(show(h4))
      val players = List("P1", "P2", "P3", "P4")
      val marginMid = s1.head.length max s3.head.length + 4
      val marginLeft = s2.head.length + 2
      val marginRight = s4.head.length
      val board =
        (1 to 14).map { i => i match {
          case 1     => " " * marginLeft + s3.pop() + " " + players(2)
          case 2|3   => " " * marginLeft + s3.pop()
          case 4     => players(1).padTo(marginLeft, " ").mkString + s3.pop()
          case 5|6   => s2.pop() + "  " + s3.pop().padTo(marginMid, " ").mkString + s4.pop()
          case 7|8   => s2.pop() + " " * (marginMid + 2) + s4.pop()
          case 9|10  => s2.pop() + "  " + s1.pop().padTo(marginMid, " ").mkString + s4.pop()
          case 11    => " " * marginLeft + s1.pop() + " " * marginRight + players(3)
          case 12|13 => " " * marginLeft + s1.pop()
          case 14    => " " * (marginLeft - 3) + players(0) + " " + s1.pop()
        }}
      assert(s1.isEmpty && s2.isEmpty && s3.isEmpty && s4.isEmpty)

      board
        .map(_.padTo(board.maxBy(_.size).size, " ").mkString)
        .zipAll(bidLog, "", "")
        .map { case (x, y) => x + "  " + y }
        .mkString("\n")
    }
  }

  object Round {
    def read(path: String): Round = {
      var h1, h2, h3, h4 = 0
      var xs = List.empty[RawBid]

      val pass = raw"P\d passes".r
      val bids = raw"P\d bids (\d+) (\d) \([a-z ]+\)".r
      val p1plays = raw"\dP1 plays (..?) (spade|diamond|heart|club)".r
      val p2plays = raw"\dP2 plays (..?) (spade|diamond|heart|club)".r
      val p3plays = raw"\dP3 plays (..?) (spade|diamond|heart|club)".r
      val p4plays = raw"\dP4 plays (..?) (spade|diamond|heart|club)".r
      val doubles = raw"P\d doubles!".r
      val redoubl = raw"P\d redoubles!".r
      val skip1 = raw"Everybody passes, \d+ \d for P\d".r
      val skip2 = raw"\dP\d wins the trick".r
      val skip3 = raw"P\d wins the last trick \(.10 pts\)".r
      val skip4 = raw"Bid successful, \d+ to \d+!".r
      val skip5 = raw"P\d scores \d+ points".r
      val skip6 = raw"P\d is now the first player".r
      val skip7 = raw"The end of the game: P\d and P\d win!".r
      val skip8 = raw"The end of the game: P\d wins!".r
      val skip9 = raw"P\d scores no point".r
      val skipA = raw"P\d says Belote!".r
      val skipB = raw"P\d says Rebelote!".r
      val skipC = raw"Bid fails, \d+ to \d+!".r
      val skipD = raw"Waiting for redouble".r
      val skipE = raw"P\d does not redouble.".r

      def parseCard(c: String): Int = (c match {
        case "A" => cA case "10" => c10 case "K" => cK case "Q" => cQ
        case "J" => cJ case "9"  => c9  case "8" => c8 case "7" => c7
        case _ => println("Unknown card? " + path); cA
      }).v

      def parseSuite(s: String): Int = (s match {
        case "spade" => spade case "diamond" => diamond
        case "heart" => heart case "club" => club
        case _ => println("Unknown suite? " + path); club
      }).v

      Source.fromFile(path).getLines.foreach {
        case pass() => xs ::= Passs
        case bids(n, c) =>
          val s = c.head match {
            case '1' => "♠" case '2' => "♥" case '3' => "♣"
            case '4' => "♦" case '5' => "TA" case '6' => "SA"
          }
          xs ::= ValueBid(n.toInt, s)
        case p1plays(c, s) => h1 |= parseCard(c) << parseSuite(s)
        case p2plays(c, s) => h2 |= parseCard(c) << parseSuite(s)
        case p3plays(c, s) => h3 |= parseCard(c) << parseSuite(s)
        case p4plays(c, s) => h4 |= parseCard(c) << parseSuite(s)
        case doubles() => xs ::= Doubles
        case redoubl() => xs ::= Redoubles
        case skip1() => case skip2() => case skip3() => case skip4() =>
        case skip5() => case skip6() => case skip7() => case skip8() =>
        case skip9() => case skipA() => case skipB() => case skipC() =>
        case skipD() => case skipE() =>
        case s => println(s"Unknown: '$s'")
      }

      Round(Hand(h1), Hand(h2), Hand(h3), Hand(h4), xs.reverse, path)
    }
  }
}
