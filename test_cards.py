import pytest

from cards import (
    C, D, H, S,
    get_playable_cards,
    get_points,
    get_trick_points,
    solve_dd_minimax,
    trick_winner,
)

# === TEST FIXTURES AND HELPERS ===

# Card Descriptors to Bitmasks
# These helpers convert human-readable card strings (e.g., "A♠")
# into the bitmask representation used by the solver.

RANKS_TRUMP = ['J', '9', 'A', '10', 'K', 'Q', '8', '7']
RANKS_PLAIN = ['A', '10', 'K', 'Q', 'J', '9', '8', '7']
CARD_TO_BIT = {}
BIT_TO_CARD = {}

bit = 31
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

# === UNIT TESTS ===

def test_card_ordering():
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
    assert get_trick_points([c("A♥"), c("K♥"), c("Q♥"), c("10♥")]) == 28
    assert get_trick_points([c("J♠"), c("9♠"), c("A♠"), c("10♠")]) == 55
    assert get_trick_points([c("7♣"), c("8♦"), c("9♥"), c("8♠")]) == 0

def test_trick_winner():
    """Tests the logic for determining the winner of a trick."""
    # Basic cases
    assert trick_winner([c("10♦"), c("K♦"), c("A♦")]) == 2
    assert trick_winner([c("10♦"), c("7♣"), c("K♦")]) == 0 # Player 1 did not follow suit

    # Trumping
    assert trick_winner([c("A♥"), c("7♠"), c("Q♥"), c("10♥")]) == 1
    assert trick_winner([c("A♥"), c("K♥"), c("7♠"), c("10♥")]) == 2
    assert trick_winner([c("A♥"), c("K♥"), c("Q♥"), c("7♠")]) == 3

    # Over-trumping
    assert trick_winner([c("A♥"), c("7♠"), c("8♠"), c("10♥")]) == 2
    assert trick_winner([c("A♥"), c("8♠"), c("7♠"), c("10♥")]) == 1

    # All-trump tricks
    assert trick_winner([c("7♠"), c("8♠"), c("9♠"), c("J♠")]) == 3
    assert trick_winner([c("J♠"), c("9♠"), c("8♠"), c("7♠")]) == 0

def test_get_playable_cards_leading():
    """Tests that all cards are playable when leading a trick."""
    hand = c("7♦,K♦,A♣,J♠")
    assert get_playable_cards([], hand) == hand

def test_get_playable_cards_following_suit():
    """Tests that players must follow the led suit."""
    trick = [c("A♦")]
    hand = c("K♦,A♣,J♠")
    assert get_playable_cards(trick, hand) == c("K♦")

def test_get_playable_cards_no_suit_can_trump():
    """Tests trumping when unable to follow suit."""
    trick = [c("A♥")]
    hand = c("J♠,7♠,9♣")
    assert get_playable_cards(trick, hand) == c("J♠,7♠")

def test_get_playable_cards_must_overtrump():
    """Tests that players must play a higher trump if possible."""
    trick = [c("A♥"), c("9♠")]
    hand = c("J♠,8♠,A♣")
    assert get_playable_cards(trick, hand) == c("J♠")

def test_get_playable_cards_cannot_overtrump():
    """Tests playing a lower trump when unable to over-trump."""
    trick = [c("A♥"), c("J♠")]
    hand = c("9♠,8♠,A♣")
    assert get_playable_cards(trick, hand) == c("9♠,8♠")

def test_get_playable_cards_partner_is_winning():
    """Tests rules for when a player's partner is winning."""
    # Partner (player 0) is winning with Ace of Hearts.
    # Player 2 must follow suit with the Queen of Hearts.
    trick = [c("A♥"), c("K♦")]
    hand = c("Q♥,J♠,A♣") # Player 2's hand
    assert get_playable_cards(trick, hand) == c("Q♥")

    # Partner (player 1) is winning with the Jack of Spades (a trump).
    # Player 3 is void in the led suit (Hearts) and is not forced to trump
    # because their partner is winning. They can play any card.
    trick = [c("A♥"), c("J♠"), c("10♥")]
    hand = c("9♠,A♣,K♣") # Player 3's hand
    assert get_playable_cards(trick, hand) == c("9♠,A♣,K♣")

def test_get_playable_cards_forced_play_no_choice():
    """Tests scenarios with only one playable card."""
    trick = [c("A♦"), c("7♣"), c("10♥")]
    hand = c("J♥,9♥")
    assert get_playable_cards(trick, hand) == c("J♥,9♥") # Must follow suit

    trick = [c("9♠"), c("7♠")]
    hand = c("J♠,8♠")
    assert get_playable_cards(trick, hand) == c("J♠") # Must over-trump

# === SOLVER (Double-Dummy) TESTS ===

def trace_play(hands, path):
    """Helper to trace the score of a game path for verification."""
    points_a = 0
    points_b = 0
    trick = []
    trick_leader = 0
    for i, (player, card) in enumerate(path):
        trick.append(card)
        if len(trick) == 4:
            winner_relative = trick_winner(trick)
            winner_absolute = (trick_leader + winner_relative) % 4
            pts = get_trick_points(trick)

            if winner_absolute % 2 == 0: # Team A
                points_a += pts
            else: # Team B
                points_b += pts
            trick = []
            trick_leader = winner_absolute
    return points_a, points_b

def test_solver_simple_endgame():
    """Tests the solver in a simple, deterministic endgame."""
    # Player 0 to lead. Team A has the two highest trumps.
    hands = [c("J♠,9♠"), 0, c("A♠,10♠"), 0]
    solve_dd_minimax.cache.clear()
    score, path = solve_dd_minimax([], hands, 0, 0)
    assert len(path) == 4
    # Team A should win all points.
    total_points = get_trick_points([c("J♠"), c("9♠"), c("A♠"), c("10♠")])
    assert score == total_points
    traced_a, traced_b = trace_play(hands, path)
    assert traced_a == total_points
    assert traced_b == 0

def test_solver_forced_trump():
    """Tests a scenario where a player is forced to trump."""
    # Player 0 leads a suit Player 1 is void in.
    hands = [c("A♥"), c("J♠"), c("K♥"), c("Q♥")]
    solve_dd_minimax.cache.clear()
    score, path = solve_dd_minimax([], hands, 0, 0)
    # Player 1 must trump, winning the trick.
    assert path[0] == (0, c("A♥"))
    assert path[1] == (1, c("J♠"))
    traced_a, traced_b = trace_play(hands, path)
    assert traced_a == 0
    assert traced_b == get_trick_points([c("A♥"), c("J♠"), c("K♥"), c("Q♥")])
    assert score == 0 # Solver returns score for team A

def test_solver_provided_scenario():
    """
    Tests the specific scenario that was included in the original tests.py file.
    This verifies the solver's output against a known complex case.
    """
    hands = [
        c("10♥,J♠,7♣,A♦"),
        c("J♥,Q♣,K♠,8♦"),
        c("9♠,A♥,7♥,10♣"),
        c("Q♠,A♣,8♠,10♦")
    ]
    solve_dd_minimax.cache.clear()
    score, path = solve_dd_minimax([], hands, 0, 0)

    # The original assertion was that Team A scores 94.
    assert score == 94

    # Verify the calculated score by replaying the game with the solver's path.
    traced_a, traced_b = trace_play(hands, path)
    assert traced_a == score
    assert traced_b == (152 - 94)

    # Optional: Print the optimal line of play for review.
    print("\n--- Optimal Play for Provided Scenario ---")
    trick = []
    trick_leader = 0
    for i, (player, card) in enumerate(path):
        print(f"Player {player} plays {d(card)}")
        trick.append(card)
        if len(trick) == 4:
            winner_relative = trick_winner(trick)
            winner_absolute = (trick_leader + winner_relative) % 4
            pts = get_trick_points(trick)
            print(f"  -> Trick won by Player {winner_absolute} for {pts} points.")
            trick = []
            trick_leader = winner_absolute
    print(f"Final Score: Team A = {traced_a}, Team B = {traced_b}")
    print("----------------------------------------")
