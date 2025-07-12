import pytest
import random
from jass import C, D, H, S

from simulation import (
    shuffle_one_hand,
    shuffle_other_hands,
    swap_trump,
    ALL_CARDS, SUIT_BIT_START
)

from test_jass import c, d
from utils import iter_bits
# --- Basic Sanity Tests ---

def test_shuffle_one_hand():
    random.seed(42)
    hand = shuffle_one_hand()
    assert isinstance(hand, int)
    assert hand.bit_count() == 9
    assert (hand & ~ALL_CARDS) == 0

def test_shuffle_other_hands_for_4_players():
    random.seed(123)
    declarer_hand = shuffle_one_hand()
    world = shuffle_other_hands(declarer_hand)

    assert isinstance(world, list)
    assert len(world) == 4

    assert sum(world) == ALL_CARDS

# --- swap_trump Tests ---

def test_swap_trump_no_change_if_spades():
    hand = (1 << 1) | (1 << 30)
    new_hands = swap_trump([hand], S)
    assert new_hands == [hand]
    assert new_hands is not [hand]

def test_swap_trump_preserves_other_suits():
    # Hand has one Club and one Diamond
    hand = (1 << 1) | (1 << 10)
    # Swap Hearts and Spades
    new_hand = swap_trump([hand], H)[0]
    # The hand should be completely unchanged
    assert new_hand == hand

def test_swap_regular_cards():
    assert swap_trump([c("A♥,6♣")], H)[0] == c("A♠,6♣")

# --- Critical Tests for Re-ordered Cards ---

@pytest.mark.parametrize("suit, card_name, non_trump_offset, trump_offset", [
    (C, "9", 3, 7),
    (D, "9", 3, 7),
    (H, "9", 3, 7),
    (C, "Jack", 5, 8),
    (D, "Jack", 5, 8),
    (H, "Jack", 5, 8),
])
def test_swap_special_card_to_trump(suit, card_name, non_trump_offset, trump_offset):
    """Tests that a non-trump 9 or Jack moves to the correct trump position."""
    # The card in its original non-trump suit
    card_bit = 1 << (SUIT_BIT_START[suit] + non_trump_offset)
    hand = card_bit

    # The expected position of that card in the Spades (trump) structure
    expected_new_card_bit = 1 << (SUIT_BIT_START[S] + trump_offset)

    # Make its suit the new trump
    new_hand = swap_trump([hand], suit)[0]

    assert new_hand == expected_new_card_bit

@pytest.mark.parametrize("target_suit, card_name, non_trump_offset, trump_offset", [
    (C, "9", 3, 7),
    (D, "9", 3, 7),
    (H, "9", 3, 7),
    (C, "Jack", 5, 8),
    (D, "Jack", 5, 8),
    (H, "Jack", 5, 8),
])
def test_swap_special_card_from_trump(target_suit, card_name, non_trump_offset, trump_offset):
    """Tests that a trump 9 or Jack moves to the correct non-trump position."""
    # The card in its original trump (Spades) position
    card_bit = 1 << (SUIT_BIT_START[S] + trump_offset)
    hand = card_bit

    # The expected position of that card in the target non-trump suit
    expected_new_card_bit = 1 << (SUIT_BIT_START[target_suit] + non_trump_offset)

    # Make the target suit the new trump (demoting Spades)
    new_hand = swap_trump([hand], target_suit)[0]

    assert new_hand == expected_new_card_bit

def test_swap_full_hand_with_special_cards():
    old_hand = c("6♣,9♣,J♦,A♥,8♠,9♠")
    new_hand = c("6♠,9♠,J♦,A♥,8♣,9♣")
    actual = swap_trump([old_hand], C)[0]
    assert actual == new_hand

    again = swap_trump([actual], C)[0]
    assert again == old_hand
