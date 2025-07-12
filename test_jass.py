from jass import (
    get_playable_cards1,
    solve_deal,
    trick_winner,
    S_NO_JACK,
    POINTS_TABLE,
    C,
    D,
    H,
    S,
    c,
    d,
)


def test_cards_representation():
    """Tests the rank hierarchy of cards."""
    assert c("J♠") > c("9♠") > c("A♠") > c("K♠") > c("Q♠") > c("10♠") > c("8♠") > c("7♠") > c("6♠")
    assert c("A♥") > c("K♥") > c("Q♥") > c("J♥") > c("10♥") > c("9♥") > c("8♥") > c("7♥") > c("6♥")
    assert c("6♣") == (1 << 0)
    assert c("J♠") == (1 << 35)
    assert S_NO_JACK == 0b011111111000000000000000000000000000

def test_get_points():
    """Tests the point values of individual cards."""
    assert POINTS_TABLE[c("A♣").bit_length()] == 11
    assert POINTS_TABLE[c("10♣").bit_length()] == 10
    assert POINTS_TABLE[c("K♣").bit_length()] == 4
    assert POINTS_TABLE[c("Q♣").bit_length()] == 3
    assert POINTS_TABLE[c("J♣").bit_length()] == 2
    assert POINTS_TABLE[c("9♣").bit_length()] == 0
    assert POINTS_TABLE[c("8♣").bit_length()] == 0
    assert POINTS_TABLE[c("7♣").bit_length()] == 0
    assert POINTS_TABLE[c("J♠").bit_length()] == 20
    assert POINTS_TABLE[c("9♠").bit_length()] == 14
    assert sum(POINTS_TABLE[c(d(1 << i)).bit_length()] for i in range(36)) == 152

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


def test_regression1():
    hands = [
        c("9♠,Q♠,8♠,6♠,A♥,8♥,8♦,7♦"),
        c("A♠,10♠,K♥,K♣,10♣,8♣,K♦,J♦"),
        c("6♥,A♣,Q♣,J♣,6♣,Q♦,10♦,9♦"),
        c("Q♥,J♥,9♥,7♥,9♣,7♣,A♦,6♦"),
    ]
    score = solve_deal(hands, transposition_table={})
    assert score == 112
