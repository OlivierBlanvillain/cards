object Play {
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
  val spades    = 24
  val diamonds  = 16
  val hearts    = 8
  val clubs     = 0

  def suite(c: Card): Suite =
    if ((c & 0xFF) != 0)
      clubs
    else if ((c & 0xFF00) != 0)
      hearts
    else if ((c & 0xFF0000) != 0)
      diamonds
    else 
      spades

  type Hand = Int
  def shuffle(): (Hand, Hand, Hand, Hand) = ???

  def followSuite(trump: Suite, suite: Suite, hand: Hand): Card = {
    val suiteInHand = hand & suite
    val trumpInHand = hand & trump
    if (suiteInHand != 0)
      randomCard(suiteInHand)
    else if(trumpInHand != 0)
      randomCard(trumpInHand)
    else
      randomCard(hand)
  }

  def randomCard(hand: Hand): Card = {
    assert(hand != 0)
    var nth = util.Random.nextInt(Integer.bitCount(hand))
    var x = 1
    while (true) {
      if ((hand & x) != 0)
        if (nth == 0)
          return x
        else
          nth -= 1
      x <<= 1
    }
  }


  def cardValue(c: Card): Int =
    if (c == cJ << spades) // assumes trump is spades
      20
    else if (c == c9 << spades)
      14
    else if ((c & 0x80808080) != 0) // cA = 0x80
      11
    else if ((c & 0x40404040) != 0) // c10 = 0x40
      10
    else if ((c & 0x20202020) != 0) // cK = 0x20
      4
    else if ((c & 0x10101010) != 0) // cQ = 0x10
      3
    else if ((c & 0x08080808) != 0) // cJ = 0x08
      2
    else
      0

  def trickValue(trump: Suite, c1: Card, c2: Card, c3: Card, c4: Card): Int = {
    val sign =
      if (
        Integer.compareUnsigned(c1, c2) > 0 &&
        Integer.compareUnsigned(c1, c4) > 0
          ||
        Integer.compareUnsigned(c3, c2) > 0 &&
        Integer.compareUnsigned(c3, c4) > 0
      ) 1 else -1

    cJ << 24
    val cA  = 0x80
    val c10 = 0x40
    val cK  = 0x20
    val cQ  = 0x10
    val cJ  = 0x08
    val c9  = 0x04
    val c8  = 0x02
    val c7  = 0x01

    type Suite = Int
    val spades    = 24
    val diamonds  = 16
    val hearts    = 8
    val clubs     = 0

    11111111
    
  }

  def randomPlay(
    trump: Suite, hands: (Hand, Hand, Hand, Hand), score: Int = 0
  ): Int = {
    if (hands._1 == 0)
      return score
    val c1 = randomCard(hands._1)
    val su = suite(c1)
    val c2 = followSuite(trump, su, hands._2)
    val c3 = followSuite(trump, su, hands._3)
    val c4 = followSuite(trump, su, hands._4)
    val newHands = (
      hands._1 ^ c1,
      hands._2 ^ c2,
      hands._3 ^ c3,
      hands._4 ^ c4
    )
    val value = trickValue(trump, c1, c2, c3, c4)
    randomPlay(trump, newHands, score + value)
  }
}
import Play._
