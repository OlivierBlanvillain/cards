from cards import (
    get_playable_cards,
    get_points,
    get_trick_points,
    double_dummy_solver,
    trick_winner,
    iter_bits,
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


def test_get_trick_points():
    """Tests the total points of a trick."""
    assert get_trick_points(tuple([c("A♥"), c("K♥"), c("Q♥"), c("10♥")])) == 28
    assert get_trick_points(tuple([c("J♠"), c("9♠"), c("A♠"), c("10♠")])) == 55
    assert get_trick_points(tuple([c("7♣"), c("8♦"), c("9♥"), c("8♠")])) == 0
    assert get_trick_points(tuple([1 << i for i in range(32)])) == 152

def test_trick_winner():
    """Tests the logic for determining the winner of a trick."""
    # Basic cases
    assert trick_winner(tuple([c("10♦"), c("K♦"), c("A♦")])) == 2
    assert trick_winner(tuple([c("10♦"), c("7♣"), c("K♦")])) == 0 # Player 1 did not follow suit

    # Trumping
    assert trick_winner(tuple([c("A♥"), c("7♠"), c("Q♥"), c("10♥")])) == 1
    assert trick_winner(tuple([c("A♥"), c("K♥"), c("7♠"), c("10♥")])) == 2
    assert trick_winner(tuple([c("A♥"), c("K♥"), c("Q♥"), c("7♠")])) == 3

    # Over-trumping
    assert trick_winner(tuple([c("A♥"), c("7♠"), c("8♠"), c("10♥")])) == 2
    assert trick_winner(tuple([c("A♥"), c("8♠"), c("7♠"), c("10♥")])) == 1

    # All-trump tricks
    assert trick_winner(tuple([c("7♠"), c("8♠"), c("9♠"), c("J♠")])) == 3
    assert trick_winner(tuple([c("J♠"), c("9♠"), c("8♠"), c("7♠")])) == 0

def test_get_playable_cards():
    """Tests the logic for playable cards."""
    # all cards are playable when leading a trick
    hand = c("7♦,K♦,A♣,J♠")
    assert get_playable_cards([], hand) == hand

    # players must follow suit
    trick = [c("A♦")]
    hand = c("K♦,A♣,J♠")
    assert get_playable_cards(trick, hand) == c("K♦")

    # must tump when unable to follow suit
    trick = [c("A♥")]
    hand = c("J♠,7♠,9♣")
    assert get_playable_cards(trick, hand) == c("J♠,7♠")

    # must play a higher trump if possible
    trick = [c("A♥"), c("9♠")]
    hand = c("J♠,8♠,A♣")
    assert get_playable_cards(trick, hand) == c("J♠")

    # must play a higher trump if possible (partner leading)
    trick = [c("9♣"), c("K♠"), c("8♠")]
    hand = c("J♠,Q♠")
    assert get_playable_cards(trick, hand) == c("J♠")

    # playing a lower trump when unable to over-trump
    trick = [c("A♥"), c("J♠")]
    hand = c("9♠,8♠,A♣")
    assert get_playable_cards(trick, hand) == c("9♠,8♠")

    # partner (player 0) is winning with Ace of Hearts
    # player 2 must follow suit with the Queen of Hearts
    trick = [c("A♥"), c("K♦")]
    hand = c("Q♥,J♠,A♣") # Player 2's hand
    assert get_playable_cards(trick, hand) == c("Q♥")

    # partner (player 1) is winning with the Jack of Spades (a trump)
    # player 3 is void in the led suit (Hearts) and is not forced to trump
    # because their partner is winning. They can play any card
    trick = [c("A♥"), c("J♠"), c("10♥")]
    hand = c("9♠,A♣,K♣") # Player 3's hand
    assert get_playable_cards(trick, hand) == c("9♠,A♣,K♣")

    # scenario where everything is playable
    trick = [c("A♦"), c("7♣"), c("10♥")]
    hand = c("J♥,9♥,8♣")
    assert get_playable_cards(trick, hand) == c("J♥,9♥,8♣")

    # if player is void in the led suit and has trumps, they must play a trump
    trick = [c("A♥")]  # Led with a Heart
    hand = c("J♠,7♠,A♣") # Player has trumps (J♠, 7♠) and a discard (A♣)
    assert get_playable_cards(trick, hand) == c("J♠,7♠")


# === SOLVER (Double-Dummy) TESTS ===

# def trace_play(hands, path):
#     """Helper to trace the score of a game path for verification."""
#     score_a = 0
#     score_b = 0
#     trick = []
#     trick_leader = 0

#     for i, (player, card) in enumerate(path):
#         trick.append(card)
#         if len(trick) == 4:
#             winner_relative = trick_winner(trick)
#             winner_absolute = (trick_leader + winner_relative) % 4
#             pts = get_trick_points(trick)

#             if winner_absolute % 2 == 0: # Team A
#                 score_a += pts
#             else: # Team B
#                 score_b += pts
#             trick = []
#             trick_leader = winner_absolute
#     return score_a - score_b

# def test_solver_simple_endgame():
#     """Tests the solver in a simple, deterministic endgame."""
#     # Player 0 to lead. Team A has the two highest trumps.
#     hands = [c("J♠,9♠"), c("7♦,8♦"), c("A♠,10♠"), c("7♥,8♥")]
#     score, path = double_dummy_solver([], hands, 0, 0, use_alpha_beta=True)
#     assert len(path) == 8
#     # Team A should win all points, so the score difference should be the total points.
#     total_points = get_trick_points([c("J♠"), c("9♠"), c("A♠"), c("10♠")])
#     assert score == total_points
#     traced_score_diff = trace_play(hands, path)
#     assert traced_score_diff == total_points

# def test_solver_forced_trump():
#     """Tests a scenario where a player is forced to trump."""
#     # Player 0 leads a suit Player 1 is void in.
#     hands = [c("A♥"), c("J♠"), c("K♥"), c("Q♥")]
#     score, path = double_dummy_solver([], hands, 0, 0, use_alpha_beta=True)
#     # Player 1 must trump, winning the trick.
#     assert path[0] == (0, c("A♥"))
#     assert path[1] == (1, c("J♠"))
#     traced_score_diff = trace_play(hands, path)
#     # Team B wins the trick, so the score difference (A - B) should be negative.
#     assert traced_score_diff == -get_trick_points([c("A♥"), c("J♠"), c("K♥"), c("Q♥")])
#     assert score == traced_score_diff


def test_minmax_alphabeta_consistency_1():
    hands = (
        c("10♥,J♠,7♣,A♦"),
        c("J♥,K♠,Q♣,8♦"),
        c("A♥,9♠,7♣,K♦"),
        c("Q♠,8♠,A♣,10♦")
    )
    score_mm = double_dummy_solver(tuple([]), hands, 0, 0, use_alpha_beta=False)
    score_ab = double_dummy_solver(tuple([]), hands, 0, 0, use_alpha_beta=True)
    assert score_ab == score_mm == 63


def test_minmax_alphabeta_consistency_2():
    hands = (
        c("10♥,J♠,7♣,A♦"),
        c("J♥,Q♣,K♠,8♦"),
        c("9♠,A♥,7♥,10♣"),
        c("Q♠,A♣,8♠,10♦")
    )
    score_mm = double_dummy_solver(tuple([]), hands, 0, 0, use_alpha_beta=False)
    score_ab = double_dummy_solver(tuple([]), hands, 0, 0, use_alpha_beta=True)
    assert score_ab == score_mm == 94
