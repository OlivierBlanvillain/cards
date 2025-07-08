from cards import (
    get_playable_cards1,
    get_playable_cards2,
    get_playable_cards3,
    get_points,
    solve_deal,
    trick_winner,
)

from test_utils import RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d


def test_cards_representation():
    """Tests the rank hierarchy of cards."""
    assert c("J♠") > c("9♠") > c("A♠") > c("10♠") > c("K♠") > c("Q♠") > c("8♠") > c("7♠")
    assert c("A♥") > c("10♥") > c("K♥") > c("Q♥") > c("J♥") > c("9♥") > c("8♥") > c("7♥")
    assert c("7♣") == (1 << 0)
    assert c("J♠") == (1 << 31)

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
    assert sum(get_points(c(d(1 << i))) for i in range(32)) == 152

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

def test_get_playable_cards():
    """Tests the logic for playable cards."""
    # players must follow suit
    hand = c("K♦,A♣,J♠")
    assert get_playable_cards1(c("A♦"), hand) == c("K♦")

    # must tump when unable to follow suit
    hand = c("J♠,7♠,9♣")
    assert get_playable_cards1(c("A♥"), hand) == c("J♠,7♠")

    # must play a higher trump if possible
    hand = c("J♠,8♠,A♣")
    assert get_playable_cards2(c("A♥"), c("9♠"), hand) == c("J♠")

    # must play a higher trump if possible (partner leading)
    hand = c("J♠,Q♠")
    assert get_playable_cards3(c("9♣"), c("K♠"), c("8♠"), hand) == c("J♠")

    # playing a lower trump when unable to over-trump
    hand = c("9♠,8♠,A♣")
    assert get_playable_cards2(c("A♥"), c("J♠"), hand) == c("9♠,8♠")

    # partner (player 0) is winning with Ace of Hearts
    # player 2 must follow suit with the Queen of Hearts
    hand = c("Q♥,J♠,A♣") # Player 2's hand
    assert get_playable_cards2(c("A♥"), c("K♦"), hand) == c("Q♥")

    # partner (player 1) is winning with the Jack of Spades (a trump)
    # player 3 is void in the led suit (Hearts) and is not forced to trump
    # because their partner is winning. They can play any card
    hand = c("9♠,A♣,K♣") # Player 3's hand
    assert get_playable_cards3(c("A♥"), c("J♠"), c("10♥"), hand) == c("9♠,A♣,K♣")

    # scenario where everything is playable
    hand = c("J♥,9♥,8♣")
    assert get_playable_cards3(c("A♦"), c("7♣"), c("10♥"), hand) == c("J♥,9♥,8♣")

    # if player is void in the led suit and has trumps, they must play a trump
    hand = c("J♠,7♠,A♣") # Player has trumps (J♠, 7♠) and a discard (A♣)
    assert get_playable_cards1(c("A♥"), hand) == c("J♠,7♠")

    # must trump if partner isn't winning
    hand = c("9♠,8♠,Q♥")
    assert get_playable_cards3(c("10♣"), c("8♦"), c("A♥"), hand) == c("9♠,8♠")


def test_minmax_alphabeta_consistency_1():
    hands = [
        c("10♥,J♠,7♣,A♦"),
        c("J♥,K♠,Q♣,8♦"),
        c("A♥,9♠,8♣,K♦"),
        c("Q♠,8♠,A♣,10♦")
    ]
    score_mm = solve_deal(hands, use_alpha_beta=False)
    score_ab = solve_deal(hands, use_alpha_beta=True)
    assert score_ab == score_mm == 63


def test_minmax_alphabeta_consistency_2():
    hands = [
        c("10♥,J♠,7♣,A♦"),
        c("J♥,Q♣,K♠,8♦"),
        c("9♠,A♥,7♥,10♣"),
        c("Q♠,A♣,8♠,10♦")
    ]
    score_mm = solve_deal(hands, use_alpha_beta=False)
    score_ab = solve_deal(hands, use_alpha_beta=True)
    assert score_ab == score_mm == 94


def test_regression0():
    hands = [
      c("8♣,J♠,Q♠,A♠,J♦,8♥,A♥,10♥"),
      c("J♣,8♠,K♠,9♠,Q♦,Q♥,7♥,J♥"),
      c("K♣,10♠,7♣,9♥,9♦,K♦,A♣,10♣"),
      c("9♣,7♠,A♦,K♥,10♦,7♦,Q♣,8♦"),
    ]
    score = solve_deal(hands, use_alpha_beta=True)
    assert score == 114
