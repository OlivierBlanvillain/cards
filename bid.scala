import scala.io.Source

object Main {
  type Card = Int
  val cA  = 0x80
  val c10 = 0x40
  val cK  = 0x20
  val cQ  = 0x10
  val cJ  = 0x08
  val c9  = 0x04
  val c8  = 0x02
  val c7  = 0x01

  type Suite = Int
  val notrump  = 40
  val alltrump = 32
  val spade    = 24
  val diamond  = 16
  val heart    = 8
  val club     = 0

  case class Hand(value: Int) extends AnyVal {
    def openInFour: Boolean = ???
    def jn3rd: Boolean = ???
    def suites: Int = ???

    def aces: Int =
      (if (contains(cA << spade)) 1 else 0) +
      (if (contains(cA << diamond)) 1 else 0) +
      (if (contains(cA << heart)) 1 else 0) +
      (if (contains(cA << club)) 1 else 0)

    def jacks: Int =
      (if (contains(cJ << spade)) 1 else 0) +
      (if (contains(cJ << diamond)) 1 else 0) +
      (if (contains(cJ << heart)) 1 else 0) +
      (if (contains(cJ << club)) 1 else 0)

    def contains(card: Card): Boolean =
      (value & card) != 0
  }

  sealed trait RawBid
  case class ValueBid(n: Int, c: Char) extends RawBid
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
        case b :: _ :: bs =>
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
      if (!check(round.h1, round.h3, normalize(round.bids)))
        println(round)
    }
  }

  def normalize(bids: List[RawBid]): List[Bid] = {
    return Nil
  }

  case class Round(h1: Hand, h2: Hand, h3: Hand, h4: Hand, bids: List[RawBid]) {
    override def toString: String = {
      def show(x: Hand): List[String] = {
        def shov(suite: Suite): String = {
          val sb = new StringBuilder()
          if (x.contains(cA << suite)) sb.append("A ")
          if (x.contains(c10 << suite)) sb.append("10 ")
          if (x.contains(cK << suite)) sb.append("K ")
          if (x.contains(cQ << suite)) sb.append("Q ")
          if (x.contains(cJ << suite)) sb.append("J ")
          if (x.contains(c9 << suite)) sb.append("9 ")
          if (x.contains(c8 << suite)) sb.append("8 ")
          if (x.contains(c7 << suite)) sb.append("7 ")
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
      def p(i: Int, lines: List[String]): List[String] =
        lines.zipWithIndex.map {
          case (l, 1) => l + " P" + i
          case (l, _) => l + "   "
        }.toList
      val s1 = p(1, show(h1))
      val s2 = p(2, show(h2))
      val s3 = p(3, show(h3))
      val s4 = p(4, show(h4))

      var bidLog = "╔════════════════╗" :: "Bidding rounds:" :: Nil
      bids.zipWithIndex.map {
        case (x, i) => (x, "P" + (i % 4 + 1))
      }.map {
        case (Passs, p) => s"║ $p passes"
        case (Doubles, p) => s"║ $p doubles"
        case (Redoubles, p) => s"║ $p redoubles"
        case (ValueBid(n, c), p) =>
          val s = c match {
            case '1' => "♠" case '2' => "♥" case '3' => "♣"
            case '4' => "♦" case '5' => "TA" case '6' => "SA"
          }
          s"║ $p bids $n $s"
      }.foreach(s => bidLog ::= s.padTo(17, " ").mkString + "║")
      bidLog ::= "╚════════════════╝"
      bidLog = bidLog.reverse

      val board =
        s3.map(" " * s2.head.length + _) ++
        s2.map(_ + " " * s1.head.length.max(s3.head.length))
          .zip(s4)
          .map { case (x, y) => x + y } ++
        s1.map(" " * s2.head.length + _)

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
      val wat = raw"The end of the game: P1, P2 and P4 win!".r

      def parseCard(c: String): Card = c match {
        case "A" => cA case "10" => c10 case "K" => cK case "Q" => cQ
        case "J" => cJ case "9"  => c9  case "8" => c8 case "7" => c7
        case _ => println("Unknown card? " + path); cA
      }

      def parseSuite(s: String): Suite = s match {
        case "spade" => spade case "diamond" => diamond
        case "heart" => heart case "club" => club
        case _ => println("Unknown suite? " + path); club
      }

      Source.fromFile(path).getLines.foreach {
        case pass() => xs ::= Passs
        case bids(n, c) => xs ::= ValueBid(n.toInt, c.head)
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
        case wat() => println(s"Unknown: '$path'")
        case s => println(s"Unknown: '$s'")
      }

      Round(Hand(h1), Hand(h2), Hand(h3), Hand(h4), xs.reverse)
    }
  }

}
