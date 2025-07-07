from cards import (
    C, D, H, S,
    get_playable_cards,
    get_points,
    get_trick_points,
    solve_dd_minimax,
    trick_winner,
    iter_bits,
)
import timeit
import statistics
import math

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


def test_benchmark_full_game():
    hands = [
        c("J♠,9♠,A♠,10♠,K♠,Q♠,8♠,7♠"),
        c("A♥,10♥,K♥,Q♥,J♥,9♥,8♥,7♥"),
        c("A♦,10♦,K♦,Q♦,J♦,9♦,8♦,7♦"),
        c("A♣,10♣,K♣,Q♣,J♣,9♣,8♣,7♣"),
    ]
    
    def run_solver():
        solve_dd_minimax(tuple([]), tuple(hands), 0, 0, use_alpha_beta=True, alpha=-999, beta=999)

    num_trials = 30
    times = []
    for _ in range(num_trials):
        solve_dd_minimax.cache_clear()
        times.append(timeit.timeit(run_solver, number=10))

    mean_time = statistics.mean(times)
    stdev_time = statistics.stdev(times)
    # For 30 trials, t-score for 95% confidence interval is 2.045 (for 29 degrees of freedom)
    confidence_interval = 2.045 * (stdev_time / math.sqrt(num_trials))

    print("\n--- 8-Trick Double Dummy Solver Benchmark ---")
    print("| Metric              | Value           |")
    print("|---------------------|-----------------|")
    print(f"| Mean Time           | {mean_time:.4f} s      |")
    print(f"| Standard Deviation  | {stdev_time:.4f} s      |")
    print(f"| 95% Confidence Int. | ±{confidence_interval:.4f} s     |")
    print("-------------------------------------")
    print("\nIndividual experiment outputs (sorted):")
    for t in sorted(times):
        print(f"- {t:.4f} s")