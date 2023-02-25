
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

  type Color = Int
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

    def show(color: Color): String = {
      val sb = new StringBuilder()
      if (contains(cA << color)) sb.append("A ")
      if (contains(c10 << color)) sb.append("10 ")
      if (contains(cK << color)) sb.append("K ")
      if (contains(cQ << color)) sb.append("Q ")
      if (contains(cJ << color)) sb.append("J ")
      if (contains(c9 << color)) sb.append("9 ")
      if (contains(c8 << color)) sb.append("8 ")
      if (contains(c7 << color)) sb.append("7 ")
      sb.toString
    }

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
  def biddingSystem = System(
     (Pass, _.aces < 2, ???),
     (P10Sw, _.aces >= 2, ???),
     (P10S A, _.aces >= 3, ???),
     (P10TA, x => x.aces >= 1 && x.jacks >= 2, ???),
     (P20Sw, _ => true, ???),
     (P20SA, _.aces == 2, ???),
     (P20TA, _.jacks >= 3, ???),
     (P30Sw, x => x.suites == 4 && x.jn3rd, ???),
     (P30SA, _.aces == 4, ???),
     (P30TA, _.jacks == 4, ???),
     (P40Sw, x => x.suites == 3 && x.jn3rd, ???),
     (P40SA, ???, ???),
     (P40TA, ???, ???),
     (P50Sw, x => x.jn3rd /*&& x.contains(Sw+1)*/, ???),
     (P50SA, ???, ???),
     (P50TA, ???, ???),
     (P60Sw, x => x.jn3rd /*&& x.contains(Sw+2)*/, ???),
     (P60SA, ???, ???),
     (P60TA, ???, ???),
     (P70Sw, x => x.jn3rd /*&& x.contains(Sw+3)*/, ???),
     (P70SA, ???, ???),
     (P70TA, ???, ???)
  )

  def check(h1: Hand, h2: Hand, starting: Boolean, bids: List[Bid]): Boolean = {
    assert(starting)

    def check0(h1: Hand, h2: Hand, bids: List[Bid], system: System): Boolean = {
      bids match {
        case Pass :: Pass :: Pass :: Pass =>
          true
        case b :: _ :: bs =>
          val (bid, pred, subsystem) = system.bids.find(_._1 == b).get
          pred(h1)
          // check0(h2, h1, bs, subsystem)
      }
    }

    check0(h1, h2, bids, biddingSystem)
  }

  def main(args: Array[String]): Unit = {
    val h1 = Hand(
      cA << spade | cA << diamond | c10 << heart | cJ << club |
      c8 << club | c9 << spade | cK << heart | c8 << diamond)

    val h2 = Hand(
     c7 << spade | cJ << diamond | cA << heart | cA << club |
     c10 << club | c10 << spade | c9 << club | cJ << spade)

    val bids = List(
      P10Sw, Pass, P10Rz, P10Sw,
      P10Rz, Pass, P30SA, Pass,
      Pass, Pass, Pass
    )

    // assert(check(h1, h2, true, bids))
    println(show(h1, h1, h2, Hand(c7 << spade), bids))
  }

  def show(x: Hand, name: String): List[String] = {
    val s = x.show(spade)
    val d = x.show(diamond)
    val h = x.show(heart)
    val c = x.show(club)
    val m = s.size max d.size max h.size max c.size
    "╔══" + "".padTo(m, "═").mkString + "═╗   " ::
    "║ ♠ " + s.padTo(m, " ").mkString + "║ " + name ::
    "║ ♥ " + d.padTo(m, " ").mkString + "║   " ::
    "║ ♣ " + h.padTo(m, " ").mkString + "║   " ::
    "║ ♦ " + c.padTo(m, " ").mkString + "║   " ::
    "╚══" + "".padTo(m, "═").mkString + "═╝   " :: Nil
  }

  def show(h1: Hand, h2: Hand, h3: Hand, h4: Hand, bids: List[Bid]): String = {
    val s1 = show(h1, "P1")
    val s2 = show(h2, "P2")
    val s3 = show(h3, "P3")
    val s4 = show(h4, "P4")
    (
      s3.map(" " * s2.head.length + _) ++
      s2.map(_ + " " * s1.head.length.max(s3.head.length))
        .zip(s4)
        .map { case (x, y) => x + y } ++
      s1.map(" " * s2.head.length + _)
    ).zipAll(
      bids.scan
      , "", ""
    ).mkString("\n")
  }
}
