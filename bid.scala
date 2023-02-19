
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
    def aces: Int =
      (if (contains(cA << spade)) 1 else 0) +
      (if (contains(cA << diamond)) 1 else 0) +
      (if (contains(cA << heart)) 1 else 0) +
      (if (contains(cA << club)) 1 else 0)

    def contains(card: Card): Boolean =
      (value & card) != 0
  }

  type Value = Int
  val plus10 = 1
  val plus20 = 2
  val plus30 = 3
  val plus40 = 4
  val plus50 = 5
  val plus60 = 6
  type Bid = (Value, Color)
  val pass = (0, 0)

  def conforms(h1: Hand, h2: Hand, starting: Boolean, bids: Seq[Bid]): Boolean = {
    if (starting) {
      bids.head match {
        case (`plus10`, color) =>
          return h1.aces >= 2
        case (`plus20`, color) =>
          return h1.aces <= 1 // only option
        case _ =>
          return true
      }
    } else
      return true
  }

  def main(args: Array[String]): Unit = {
    val h1 = Hand(
      cA << spade | cA << diamond | c10 << heart | cJ << club |
      c8 << club | c9 << spade | cK << heart | c8 << diamond)

    val h2 = Hand(
     c7 << spade | cJ << diamond | cA << heart | cA << club |
     c10 << club | c10 << spade | c9 << club | cJ << spade)

    val bid = List(
      (plus10, club),
      pass,
      (plus10, club),
      (plus10, heart),
      (plus10, club),
      pass,
      (plus30, notrump),
      pass,
      pass,
      pass,
      pass
    )

    assert(conforms(h1, h2, true, bid))
  }
}
