from analysis import pretty_print_cards, iter_bits, analyze_game, print_mistake_report
from utils import c, d
import pytest
from unittest.mock import patch, DEFAULT
import io
from contextlib import redirect_stdout
import sys
import subprocess

def test_iter_bits():
    assert list(iter_bits(0b1011)) == [0b1, 0b10, 0b1000]
    assert list(iter_bits(0)) == []
    assert list(iter_bits(0b1)) == [0b1]

def test_pretty_print_cards():
    assert pretty_print_cards(c('7♣,9♣')) == "9♣, 7♣"
    assert pretty_print_cards(0) == ""
    assert pretty_print_cards(c('A♠')) == "A♠"

def test_analyze_game_with_mistakes():
    game_hands = [
      "8♣,J♠,Q♠,A♠,J♦,8♥,A♥,10♥",
      "J♣,8♠,K♠,9♠,Q♦,Q♥,7♥,J♥",
      "K♣,10♠,7♣,9♥,9♦,K♦,A♣,10♣",
      "9♣,7♠,A♦,K♥,10♦,7♦,Q♣,8♦",
    ]
    game_tricks = [
      ["8♣", "J♣", "K♣", "9♣"],
      ["10♠", "7♠", "J♠", "8♠"],
      ["Q♠", "K♠", "7♣", "A♦"],
      ["9♠", "9♥", "K♥", "A♠"],
      ["Q♦", "9♦", "10♦", "J♦"],
      ["7♦", "8♥", "Q♥", "K♦"],
      ["A♣", "Q♣", "A♥", "7♥"],
      ["10♣", "8♦", "10♥", "J♥"],
    ]
    f = io.StringIO()
    with redirect_stdout(f):
        analyze_game(game_hands, game_tricks)
    output = f.getvalue()
    assert "MISTAKE DETECTED" in output

def test_print_mistake_report():
    f = io.StringIO()
    with redirect_stdout(f):
        hands = [c("A♠,K♠"), c("Q♠,J♠"), c("10♠,9♠"), c("8♠,7♠")]
        trick = [c("8♣"), c("J♣"), c("K♣")]
        trick_leader = 0
        current_player = 3
        played_card = c("9♣")
        optimal_moves = [c("7♣"), c("10♣")]
        points_lost = 5.0
        trick_idx = 0
        card_idx_in_trick = 3
        print_mistake_report(hands, trick, trick_leader, current_player, played_card, optimal_moves, points_lost, trick_idx, card_idx_in_trick)
    output = f.getvalue()

    assert "MISTAKE DETECTED on Trick 1 (Player 3)" in output
    assert "Player 0's hand: A♠, K♠" in output
    assert "Current trick: [8♣, J♣, K♣]" in output
    assert "Move played        : 9♣" in output
    assert "Optimal move(s)    : 10♣, 7♣" in output
    assert "Points lost        : 5" in output
