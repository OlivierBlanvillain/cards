import scala.io.Source
import collection.mutable
import scala.language.implicitConversions

// TODO: fst/25058168111

object Main {
  case class Card(v: Int) extends AnyVal {
    override def toString: String = Hand(v).toString
  }

  case class CardValue(v: Int) extends AnyVal {
    def of(s: Suite): Card = Card(v << s.v)
  }
  val A   = CardValue(0x80)
  val K   = CardValue(0x20)
  val Q   = CardValue(0x10)
  val J   = CardValue(0x08)
  implicit def conv10(i: 10): CardValue = CardValue(0x40)
  implicit def conv9(i: 9): CardValue = CardValue(0x04)
  implicit def conv8(i: 8): CardValue = CardValue(0x02)
  implicit def conv7(i: 7): CardValue = CardValue(0x01)

  case class Suite(v: Int) extends AnyVal {
    override def toString: String =
      (this: @unchecked) match {
        case SA => "SA"
        case TA => "TA"
        case S => "♠"
        case H => "♥"
        case C => "♣"
        case D => "♦"
      }

    def next1: Suite =
      (this: @unchecked) match {
        case SA => SA
        case TA => TA
        case S => H
        case H => C
        case C => D
        case D => S
      }
    def next2: Suite = next1.next1
    def next3: Suite = next2.next1

  }
  val SA = Suite(40)
  val TA = Suite(32)
  val S = Suite(24)
  val H = Suite(16)
  val C = Suite(8)
  val D = Suite(0)

  object SU {
    def unapply(s: Suite): Option[Suite] = s match {
      case SA => None
      case TA => None
      case _ => Some(s)
    }
  }

  case class Hand(v: Int) extends AnyVal {
    def JN3(s: Suite): Boolean =
      JN(s) && count(s) >= 3

    def JN(s: Suite): Boolean =
      contains(J of s) && contains(9 of s)

    def KQ(s: Suite): Boolean =
      contains(K of s) && contains(Q of s)

    def JNorJ3orN3orKQ3or5(s: Suite): Boolean =
      JN(s) || (JorN(s) || KQ(s)) && count(s) >= 3 || count(s) >= 5

    def JorN2(s: Suite): Boolean =
      contains(J of s) || contains(9 of s) && count(s) >= 2

    def JorN(s: Suite): Boolean =
      contains(J of s) || contains(9 of s)

    def count(s: Suite): Int =
      c(A of s) + c(10 of s) + c(K of s) + c(Q of s) +
      c(J of s) + c(9 of s)  + c(8 of s) + c(7 of s)

    def suites: Int =
      List(S, H, C, D).map(count).count(0.!=)

    def aces: Int =
      c(A of S) + c(A of H) + c(A of C) + c(A of D)

    def acesOustide(s: Suite): Int =
      c(A of S) + c(A of H) + c(A of C) + c(A of D) - c(A of s)

    def jacks: Int =
      c(J of S) + c(J of H) + c(J of C) + c(J of D)

    def c(card: Card): Int =
      if (contains(card)) 1 else 0

    def contains(card: Card): Boolean =
      (v & card.v) != 0

    override def toString: String = {
      def shov(s: Suite): String = {
        val sb = new StringBuilder()
        if (contains(A of s)) sb.append("A ")
        if (contains(10 of s)) sb.append("10 ")
        if (contains(K of s)) sb.append("K ")
        if (contains(Q of s)) sb.append("Q ")
        if (contains(J of s)) sb.append("J ")
        if (contains(9 of s)) sb.append("9 ")
        if (contains(8 of s)) sb.append("8 ")
        if (contains(7 of s)) sb.append("7 ")
        sb.toString
      }
      val s = shov(S)
      val d = shov(D)
      val h = shov(H)
      val c = shov(C)
      val m = s.size max d.size max h.size max c.size
      "╔══" + "".padTo(m, "═").mkString + "═╗\n" +
      "║ ♠ " + s.padTo(m, " ").mkString + "║\n" +
      "║ ♥ " + h.padTo(m, " ").mkString + "║\n" +
      "║ ♣ " + c.padTo(m, " ").mkString + "║\n" +
      "║ ♦ " + d.padTo(m, " ").mkString + "║\n" +
      "╚══" + "".padTo(m, "═").mkString + "═╝"
    }
  }

  sealed trait AbsoluteBid
  case class Bidd(n: Int, s: Suite) extends AbsoluteBid
  case object Passs extends AbsoluteBid
  case object Contre extends AbsoluteBid
  case object Surcontre extends AbsoluteBid

  sealed trait RelativeBid
  case class Bid(relative: Int, suite: Suite) extends RelativeBid
  case class Raise(relative: Int)(val fighting: Boolean) extends RelativeBid
  case object Pass extends RelativeBid
  case object Capot extends RelativeBid

  case class System(val next: PartialFunction[RelativeBid, (Hand => Boolean, System)]) {
    // def using(next: RelativeBid => (Hand => Boolean, System)): System = ???
    def either(other: System): System = ???
  }

  // As player 1
  def always(h: Hand) = true
  def never(h: Hand) = false
  def signoff: System = System { case _ => (always, signoff) }
  val BIDDING_SPACE: (Hand => Boolean, System) = (never, signoff)
  val TODO = signoff

  def raiseWith(card: Card) = System {
    case Raise(_) => (_.contains(card), signoff)
    case _ => (always, signoff)
  }

  // case Raise(20)
  def noComplement2As(s: Suite): (Hand => Boolean, System) =
    (!_.JorN2(s), System {
      case Pass => (always, signoff)
      case Raise(10) => (_.JN(s), System {
        case Pass      => (always, signoff)
        case Raise(10) => (_.acesOustide(s) == 1, TODO)
        case Raise(20) => (_.acesOustide(s) == 2, TODO)
        case Bid(10, SA) => (_.contains(A of s), TODO)
        case Raise(_)  => BIDDING_SPACE
        case Bid(_, _) => BIDDING_SPACE
        case Capot => (always, signoff)
      })
      case Bid(10, SA) => (always, System {
        case Pass      => (_.aces == 0, signoff)
        case Raise(10) => (_.aces == 1, TODO)
        case Raise(20) => (_.aces == 2, TODO)
        case Raise(_)  => BIDDING_SPACE
        case Bid(_, _) => BIDDING_SPACE
        case Capot => (always, signoff)
      })
      case Bid(20, SA) => (_.aces == 3, System {
        case Pass      => (_.aces == 0, signoff)
        case Raise(10) => (_.aces == 1, signoff)
        case Raise(_)  => BIDDING_SPACE
        case Bid(_, _) => BIDDING_SPACE
      })
      case Raise(_)  => BIDDING_SPACE // +20 could say mean JN + openIn4, or
      case Bid(_, _) => BIDDING_SPACE
      case Capot => (always, signoff)
    })

  lazy val system = System {
    case Pass => (always, TODO)
    case Bid(10, SA) => (_.aces >= 3, TODO)
    case Bid(10, TA) => (x => x.aces >= 1 && x.jacks >= 2, TODO)
    case Bid(10, SU(s)) => (_.aces >= 2, System {
      case Raise(10) => ( _.JorN2(s), TODO)
      case Raise(20) => noComplement2As(s)
      case Raise(30) => (_.JN(s), TODO)
      // case Raise(_) => (never, TODO)
      case Bid(10, SU(s)) => (_.JNorJ3orN3orKQ3or5(s), System {
        case Raise(10) => (_.JorN(s), TODO)
        case Raise(20) => noComplement2As(s)
        case _ => (always, TODO)
      })
      // case Bid(20) => (x => ..., TODO)
      // case Bid(30) => (x => ..., TODO)
      // case Bid(40) => (x => ..., TODO)
      // case Bid(50) => (x => ..., TODO)
      case _ => (always, TODO)
    })
    case Bid(20, SA) => (_.aces == 2, TODO)
    case Bid(20, TA) => (_.jacks >= 3, TODO)
    case Bid(20, SU(s)) => (_.JNorJ3orN3orKQ3or5(s), System {
      case Raise(10) => (_.JorN(s), TODO)
      case Raise(20) => (!_.JorN(s), TODO)
      case Raise(30) => (x => x.JorN(s) && x.aces >= 2, TODO)
      case Raise(40) => (x => x.JorN(s) && x.aces >= 3, TODO)
      case Raise(_) => (never, TODO)
      case Bid(10, SA) => (_.aces >= 2, TODO)
      case Bid(20, SA) => (_.aces >= 3, TODO)
      case Bid(30, SA) => (_.aces == 4, TODO)
      case Bid(_, SA) => (never, TODO)
      // case Bid(10, Su(s)) => (, TODO)
      // case Bid(20, Su(s)) => (, TODO)
      // case Bid(30, Su(s)) => (, TODO)
      case Bid(_, SU(s)) => (never, TODO)
      case _ => (always, TODO)
    })
    case Bid(30, SA) => (_.aces == 4, TODO)
    case Bid(30, TA) => (_.jacks == 4, TODO)
    case Bid(30, SU(s)) => (x => x.suites == 4 && x.JN3(s), TODO)
      // +10) => asks for 5th and 6th trick TA
    case Bid(40, SA) => (always, raiseWith(10 of S))
    case Bid(50, SA) => (always, raiseWith(10 of H))
    case Bid(60, SA) => (always, raiseWith(10 of C))
    case Bid(70, SA) => (always, raiseWith(10 of D))

    case Bid(40, TA) => (always, raiseWith(9 of S))
    case Bid(50, TA) => (always, raiseWith(9 of H))
    case Bid(60, TA) => (always, raiseWith(9 of C))
    case Bid(70, TA) => (always, raiseWith(9 of D))

    case Bid(40, SU(su)) => (x => x.JN3(su) && x.suites == 3, System {
      case r @ Raise(_) if !r.fighting => (x => x.aces - x.c(A of su) >= 2, signoff)
      case Raise(_) => (always, signoff)
      case Pass => (always, signoff)
      case Capot => (always, signoff)
      case Bid(_, TA) => (_.jacks >= 2, signoff)
      case Bid(_, SA) => (x => x.aces >= 2 && x.contains(A of su), signoff)
      case Bid(_, sv) => (_.JN3(sv), signoff)
    })
    case Bid(50, SU(su)) => (x => x.JN3(su) && x.count(su.next1) >= 0, raiseWith(A of su.next1))
    case Bid(60, SU(su)) => (x => x.JN3(su) && x.count(su.next2) >= 0, raiseWith(A of su.next2))
    case Bid(70, SU(su)) => (x => x.JN3(su) && x.count(su.next3) >= 0, raiseWith(A of su.next3))

    case Capot => (always, signoff)
  }

  def check(h1: Hand, h2: Hand, bids: List[RelativeBid], system: System, round: Round): Unit = {
    (bids: @unchecked) match {
      case Nil =>
      case b :: bs =>
        if (!system.next.isDefinedAt(b)) {
          println(s"\nMissing contract definition:")
          println(s"$b :: $bs")
          println(s"${round.path}")
          println(round)
        } else system.next(b) match {
          case (pred, subsystem) =>
            if (!pred(h1)) {
              println(s"\nContract violation:")
              println(s"$b :: $bs")
              println(s"${round.path}")
              println(round)
            } else
              check(h2, h1, bs, subsystem, round)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    args.foreach { path =>
      val round = Round.read(path)
      println(path)
      println(round)
      // val normalized = normalize(round.bids, 70, Suite(99), false)
      // check(round.h1, round.h3, normalized, system, round)
    }
  }

  def normalize(bids: List[AbsoluteBid], lb: Int, ls: Suite, fighting: Boolean): List[RelativeBid] = {
    def max(prev: Int, opponentBid: AbsoluteBid): Int =
      opponentBid match {
        case Bidd(n, _) => n
        case _ => prev
      }

    def next(value: Int, suite: Suite): RelativeBid =
      if (suite == ls)
        Raise(value - lb)(fighting)
      else
        Bid(value - lb, suite)

    bids match {
      case Nil => Nil
      case Contre :: _ => Nil
      case Surcontre :: _ => Nil
      case Passs :: Nil => Pass :: Nil

      case Passs :: ob :: xs =>
        Pass :: normalize(xs, max(lb, ob), ls, fighting || ob != Passs)

      case Bidd(250, _) :: _ =>
        Capot :: Nil

      case Bidd(n, s) :: ob :: xs =>
        next(n, s) :: normalize(xs, max(n, ob), s, ob != Passs)

      case _ =>
        throw new IllegalStateException(bids.toString)
    }
  }

  case class Round(
    h1: Hand,
    h2: Hand,
    h3: Hand,
    h4: Hand,
    bids: List[AbsoluteBid],
    path: String
  ) {
    override def toString: String = {
      val plus1 = bids.exists {
        case Bidd(n, _) => n >= 100
        case _ => true
      }
      var bidLog = "╔════════════════╗" :: Nil
      bids.zipWithIndex.map {
        case (x, i) => (x, "P" + (i % 4 + 1))
      }.map {
        case (Passs, p) => s"║ $p passes"
        case (Contre, p) => s"║ $p doubles"
        case (Surcontre, p) => s"║ $p redoubles"
        case (Bidd(n, s), p) => s"║ $p bids $n $s"
      }.foreach(s => bidLog ::= s.padTo(16 + (if (plus1) 1 else 0), " ").mkString + "║")
      bidLog ::= "╚════════════════╝"
      bidLog = bidLog.reverse

      val s1 = mutable.Stack.from(h1.toString.split("\n").toList)
      val s2 = mutable.Stack.from(h2.toString.split("\n").toList)
      val s3 = mutable.Stack.from(h3.toString.split("\n").toList)
      val s4 = mutable.Stack.from(h4.toString.split("\n").toList)
      val players = List("P1", "P2", "P3", "P4")
      val marginMid = s1.head.length.max(s3.head.length) + 1
      val marginLeft = s2.head.length + 1
      val marginRight = (s3.head.length - s1.head.length).max(0) + s4.head.length - 1
      val board =
        (1 to 12).map { i => i match {
          case 1     => " " * marginLeft + s3.pop() + " " + players(2)
          case 2     => " " * marginLeft + s3.pop()
          case 3     => players(1).padTo(marginLeft, " ").mkString + s3.pop()
          case 4|5|6 => s2.pop() + " " + s3.pop().padTo(marginMid, " ").mkString + s4.pop()
          case 7|8|9 => s2.pop() + " " + s1.pop().padTo(marginMid, " ").mkString + s4.pop()
          case 10    => " " * marginLeft + s1.pop() + " " * marginRight + players(3)
          case 11    => " " * marginLeft + s1.pop()
          case 12    => " " * (marginLeft - 3) + players(0) + " " + s1.pop()
        }}
      assert(s1.isEmpty && s2.isEmpty && s3.isEmpty && s4.isEmpty)

      board
        .zipAll(bidLog, "", "")
        .map { case (x, y) =>
          x.padTo(board.maxBy(_.size).size, " ").mkString + "  " + y
        }
        .mkString("\n")
    }
  }

  object Round {
    def read(path: String): Round = {
      var h1, h2, h3, h4 = 0
      var xs = List.empty[AbsoluteBid]

      val pass = raw"P\d passes".r
      val bids = raw"P\d bids (\d+) (\d) \([a-z ]+\)".r
      val p1plays = raw"\dP1 plays (..?) (spade|heart|club|diamond)".r
      val p2plays = raw"\dP2 plays (..?) (spade|heart|club|diamond)".r
      val p3plays = raw"\dP3 plays (..?) (spade|heart|club|diamond)".r
      val p4plays = raw"\dP4 plays (..?) (spade|heart|club|diamond)".r
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

      def parseCard(c: String): CardValue = c match {
        case "A" => A case "10" => 10 case "K" => K case "Q" => Q
        case "J" => J case "9"  => 9  case "8" => 8 case "7" => 7
      }

      def parseSuite(s: String): Suite = s match {
        case "spade" => S
        case "heart" => H
        case "club" => C
        case "diamond" => D
      }

      Source.fromFile(path).getLines.foreach {
        case pass() => xs ::= Passs
        case bids(n, c) =>
          val s: Suite = c.head match {
            case '1' => S case '2' => H case '3' => C
            case '4' => D case '5' => TA case '6' => SA
          }
          xs ::= Bidd((n.toInt / 10) * 10, s)
        case p1plays(c, s) => h1 |= parseCard(c).of(parseSuite(s)).v
        case p2plays(c, s) => h2 |= parseCard(c).of(parseSuite(s)).v
        case p3plays(c, s) => h3 |= parseCard(c).of(parseSuite(s)).v
        case p4plays(c, s) => h4 |= parseCard(c).of(parseSuite(s)).v
        case doubles() => xs ::= Contre
        case redoubl() => xs ::= Surcontre
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
