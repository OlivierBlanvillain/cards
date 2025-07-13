from jass import (
    get_playable_cards,
    solve_deal,
    trick_winner,
    POINTS_TABLE,
    C, D, H, S, F, c, d
)


def test_cards_representation():
    """Tests the rank hierarchy of cards."""
    assert c("JS") > c("9S") > c("AS") > c("KS") > c("QS") > c("10S") > c("8S") > c("7S") > c("6S")
    assert c("AH") > c("KH") > c("QH") > c("JH") > c("10H") > c("9H") > c("8H") > c("7H") > c("6H")
    assert c("6C") == (1 << 0)
    assert c("JS") == (1 << 35)
    assert F == 0b011111111000000000000000000000000000

def test_get_points():
    """Tests the point values of individual cards."""
    assert POINTS_TABLE[c("AC").bit_length()] == 11
    assert POINTS_TABLE[c("10C").bit_length()] == 10
    assert POINTS_TABLE[c("KC").bit_length()] == 4
    assert POINTS_TABLE[c("QC").bit_length()] == 3
    assert POINTS_TABLE[c("JC").bit_length()] == 2
    assert POINTS_TABLE[c("9C").bit_length()] == 0
    assert POINTS_TABLE[c("8C").bit_length()] == 0
    assert POINTS_TABLE[c("7C").bit_length()] == 0
    assert POINTS_TABLE[c("JS").bit_length()] == 20
    assert POINTS_TABLE[c("9S").bit_length()] == 14
    assert sum(POINTS_TABLE[c(d(1 << i)).bit_length()] for i in range(36)) == 152

def test_trick_winner():
    """Tests the logic for determining the winner of a trick."""
    # Trumping
    assert trick_winner(c("AH"), c("7S"), c("QH"), c("10H")) == 1
    assert trick_winner(c("AH"), c("KH"), c("7S"), c("10H")) == 2
    assert trick_winner(c("AH"), c("KH"), c("QH"), c("7S")) == 3

    # Over-trumping
    assert trick_winner(c("AH"), c("7S"), c("8S"), c("10H")) == 2
    assert trick_winner(c("AH"), c("8S"), c("7S"), c("10H")) == 1

    # All-trump tricks
    assert trick_winner(c("7S"), c("8S"), c("9S"), c("JS")) == 3
    assert trick_winner(c("JS"), c("9S"), c("8S"), c("7S")) == 0

    # wierd 10 stuff
    assert trick_winner(c("7H"), c("8H"), c("9H"), c("10H")) == 3
    assert trick_winner(c("7H"), c("8H"), c("10H"), c("JH")) == 3


def test_get_playable_cards():
    """Tests the logic for playable cards."""
    # players must follow suit or play JS
    hand = c("KD,AC,JS")
    assert get_playable_cards(c("AD"), hand) == c("KD,JS")

    # players must follow the trump suit
    hand = c("KD,AS,JS")
    assert get_playable_cards(c("6S"), hand) == c("AS,JS")

    # players are never force to play their JS
    hand = c("KD,JS")
    assert get_playable_cards(c("6S"), hand) == c("KD,JS")

    # free to play any cards when unable to follow suit
    hand = c("JS,7S,9C")
    assert get_playable_cards(c("AH"), hand) == c("JS,7S,9C")

    # free to play lower trump
    hand = c("JS,8S,AC")
    assert get_playable_cards(c("AH"), hand) == c("JS,8S,AC")
    hand = c("JS,QS")
    assert get_playable_cards(c("9C"), hand) == c("JS,QS")
    hand = c("9S,8S,AC")
    assert get_playable_cards(c("AH"), hand) == c("9S,8S,AC")

    # follow suit with the Queen of Hearts or JS
    hand = c("QH,JS,AC")
    assert get_playable_cards(c("AH"), hand) == c("QH,JS")

    # player 3 is void in the led suit (Hearts) and is not forced to trump
    hand = c("9S,AC,KC")
    assert get_playable_cards(c("AH"), hand) == c("9S,AC,KC")

    # scenario where everything is playable
    hand = c("JH,9H,8C")
    assert get_playable_cards(c("AD"), hand) == c("JH,9H,8C")

    # if player is void in the led suit and has trumps, they are free to play anything
    hand = c("JS,7S,AC") # Player has trumps (JS, 7S) and a discard (AC)
    assert get_playable_cards(c("AH"), hand) == c("JS,7S,AC")

    # free to play anything if partner isn't winning
    hand = c("9S,8S,QH")
    assert get_playable_cards(c("10C"), hand) == c("9S,8S,QH")


def test_minmax_alphabeta_consistency_1():
    hands = [
        c("10H,JS,7C,AD"),
        c("JH,KS,QC,8D"),
        c("AH,9S,8C,KD"),
        c("QS,8S,AC,10D")
    ]
    score = solve_deal(hands)
    assert score == 94


def test_minmax_alphabeta_consistency_2():
    hands = [
        c("10H,JS,7C,AD"),
        c("JH,QC,KS,8D"),
        c("9S,AH,7H,10C"),
        c("QS,AC,8S,10D")
    ]
    score = solve_deal(hands)
    assert score == 100


def test_regression_1():
    hands = [
      c("8C,JS,QS,AS,JD,8H,AH,10H"),
      c("JC,8S,KS,9S,QD,QH,7H,JH"),
      c("KC,10S,7C,9H,9D,KD,AC,10C"),
      c("9C,7S,AD,KH,10D,7D,QC,8D"),
    ]
    score = solve_deal(hands)
    assert score == 112


def test_regression_2():
    hands = [
        c("9S,QS,8S,6S,AH,8H,8D,7D"),
        c("AS,10S,KH,KC,10C,8C,KD,JD"),
        c("6H,AC,QC,JC,6C,QD,10D,9D"),
        c("QH,JH,9H,7H,9C,7C,AD,6D"),
    ]
    score = solve_deal(hands)
    assert score == 83
