from jass import (
    get_playable_cards1,
    get_points,
    solve_deal,
    trick_winner,
    S_NO_JACK,
)

from jass import C, D, H, S

RANKS_TRUMP = ['J', '9', 'A', 'K', 'Q', '10', '8', '7', '6']
RANKS_PLAIN = ['A', 'K', 'Q', 'J', '10', '9', '8', '7', '6']
CARD_TO_BIT = {}
BIT_TO_CARD = {}

bit = 35
for suit, suit_name in [(S, "♠"), (H, "♥"), (D, "♦"), (C, "♣")]:
    ranks = RANKS_TRUMP if suit == S else RANKS_PLAIN
    for rank in ranks:
        CARD_TO_BIT[(suit, rank)] = bit
        BIT_TO_CARD[bit] = (suit, rank, suit_name)
        bit -= 1

def c(desc: str) -> int:
    """Converts a comma-separated string of cards (e.g., 'A♠,K♥') to a bitmask."""
    total = 0
    if not desc:
        return total
    for token in desc.split(','):
        rank = token[:-1]
        suit_char = token[-1]
        suit = {"♣": C, "♦": D, "♥": H, "♠": S}[suit_char]
        total |= 1 << CARD_TO_BIT[(suit, rank)]
    return total

def d(card_mask: int) -> str:
    """Converts a single card bitmask back to its string representation (e.g., 'A♠')."""
    if card_mask == 0:
        return ""
    bit_pos = card_mask.bit_length() - 1
    suit, rank, suit_char = BIT_TO_CARD[bit_pos]
    return rank + suit_char



def test_cards_representation():
    """Tests the rank hierarchy of cards."""
    assert c("J♠") > c("9♠") > c("A♠") > c("K♠") > c("Q♠") > c("10♠") > c("8♠") > c("7♠") > c("6♠")
    assert c("A♥") > c("K♥") > c("Q♥") > c("J♥") > c("10♥") > c("9♥") > c("8♥") > c("7♥") > c("6♥")
    assert c("6♣") == (1 << 0)
    assert c("J♠") == (1 << 35)
    assert S_NO_JACK == 0b011111111000000000000000000000000000

def test_get_points():
    """Tests the point values of individual cards."""
    assert get_points(c("A♣")) == 11
    assert get_points(c("10♣")) == 10
    assert get_points(c("K♣")) == 4
    assert get_points(c("Q♣")) == 3
    assert get_points(c("J♣")) == 2
    assert get_points(c("9♣")) == 0
    assert get_points(c("8♣")) == 0
    assert get_points(c("7♣")) == 0
    assert get_points(c("J♠")) == 20
    assert get_points(c("9♠")) == 14
    assert sum(get_points(c(d(1 << i))) for i in range(36)) == 152

def test_trick_winner():
    """Tests the logic for determining the winner of a trick."""
    # Trumping
    assert trick_winner(c("A♥"), c("7♠"), c("Q♥"), c("10♥")) == 1
    assert trick_winner(c("A♥"), c("K♥"), c("7♠"), c("10♥")) == 2
    assert trick_winner(c("A♥"), c("K♥"), c("Q♥"), c("7♠")) == 3

    # Over-trumping
    assert trick_winner(c("A♥"), c("7♠"), c("8♠"), c("10♥")) == 2
    assert trick_winner(c("A♥"), c("8♠"), c("7♠"), c("10♥")) == 1

    # All-trump tricks
    assert trick_winner(c("7♠"), c("8♠"), c("9♠"), c("J♠")) == 3
    assert trick_winner(c("J♠"), c("9♠"), c("8♠"), c("7♠")) == 0

    # wierd 10 stuff
    assert trick_winner(c("7♥"), c("8♥"), c("9♥"), c("10♥")) == 3
    assert trick_winner(c("7♥"), c("8♥"), c("10♥"), c("J♥")) == 3


def test_get_playable_cards():
    """Tests the logic for playable cards."""
    # players must follow suit or play J♠
    hand = c("K♦,A♣,J♠")
    assert get_playable_cards1(c("A♦"), hand) == c("K♦,J♠")

    # players must follow the trump suit
    hand = c("K♦,A♠,J♠")
    assert get_playable_cards1(c("6♠"), hand) == c("A♠,J♠")

    # players are never force to play their J♠
    hand = c("K♦,J♠")
    assert get_playable_cards1(c("6♠"), hand) == c("K♦,J♠")

    # free to play any cards when unable to follow suit
    hand = c("J♠,7♠,9♣")
    assert get_playable_cards1(c("A♥"), hand) == c("J♠,7♠,9♣")

    # free to play lower trump
    hand = c("J♠,8♠,A♣")
    assert get_playable_cards1(c("A♥"), hand) == c("J♠,8♠,A♣")
    hand = c("J♠,Q♠")
    assert get_playable_cards1(c("9♣"), hand) == c("J♠,Q♠")
    hand = c("9♠,8♠,A♣")
    assert get_playable_cards1(c("A♥"), hand) == c("9♠,8♠,A♣")

    # follow suit with the Queen of Hearts or J♠
    hand = c("Q♥,J♠,A♣")
    assert get_playable_cards1(c("A♥"), hand) == c("Q♥,J♠")

    # player 3 is void in the led suit (Hearts) and is not forced to trump
    hand = c("9♠,A♣,K♣")
    assert get_playable_cards1(c("A♥"), hand) == c("9♠,A♣,K♣")

    # scenario where everything is playable
    hand = c("J♥,9♥,8♣")
    assert get_playable_cards1(c("A♦"), hand) == c("J♥,9♥,8♣")

    # if player is void in the led suit and has trumps, they are free to play anything
    hand = c("J♠,7♠,A♣") # Player has trumps (J♠, 7♠) and a discard (A♣)
    assert get_playable_cards1(c("A♥"), hand) == c("J♠,7♠,A♣")

    # free to play anything if partner isn't winning
    hand = c("9♠,8♠,Q♥")
    assert get_playable_cards1(c("10♣"), hand) == c("9♠,8♠,Q♥")


def test_minmax_alphabeta_consistency_1():
    hands = [
        c("10♥,J♠,7♣,A♦"),
        c("J♥,K♠,Q♣,8♦"),
        c("A♥,9♠,8♣,K♦"),
        c("Q♠,8♠,A♣,10♦")
    ]
    score = solve_deal(hands, transposition_table={})
    assert score == 94


def test_minmax_alphabeta_consistency_2():
    hands = [
        c("10♥,J♠,7♣,A♦"),
        c("J♥,Q♣,K♠,8♦"),
        c("9♠,A♥,7♥,10♣"),
        c("Q♠,A♣,8♠,10♦")
    ]
    score = solve_deal(hands, transposition_table={})
    assert score == 100


def test_regression0():
    hands = [
      c("8♣,J♠,Q♠,A♠,J♦,8♥,A♥,10♥"),
      c("J♣,8♠,K♠,9♠,Q♦,Q♥,7♥,J♥"),
      c("K♣,10♠,7♣,9♥,9♦,K♦,A♣,10♣"),
      c("9♣,7♠,A♦,K♥,10♦,7♦,Q♣,8♦"),
    ]
    score = solve_deal(hands, transposition_table={})
    assert score == 112
