#!venv/bin/python3

from cards import double_dummy_solver0, _cache
import timeit
import statistics
import math

from test_utils import RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d

def test_benchmark_full_game():
    hands = [
        c("10♠,A♥,10♥,9♥,7♥,10♦,J♦,10♣"),
        c("A♠,9♠,Q♥,J♥,8♥,8♦,A♣,7♣"),
        c("K♠,J♠,K♥,K♦,9♦,7♦,K♣,Q♣"),
        c("Q♠,8♠,7♠,A♦,Q♦,J♣,9♣,8♣"),
    ]

    def run_solver():
        double_dummy_solver0(hands, 0, use_alpha_beta=True)

    num_trials = 30
    times = []
    for _ in range(num_trials):
        _cache.clear()
        times.append(timeit.timeit(run_solver, number=10))

    mean_time = statistics.mean(times)
    stdev_time = statistics.stdev(times)
    # For 30 trials, t-score for 95% confidence interval is 2.045 (for 29 degrees of freedom)
    confidence_interval = 2.045 * (stdev_time / math.sqrt(num_trials))

    print()
    print("|---------------------|---------------|")
    print("| Metric              | Value         |")
    print("|---------------------|---------------|")
    print(f"| Mean Time           | {mean_time:.4f} s      |")
    print(f"| Standard Deviation  | {stdev_time:.4f} s      |")
    print(f"| 95% Confidence Int. | ±{confidence_interval:.4f} s     |")
    print("|-------------------------------------|")
    print("\nIndividual experiment outputs (sorted):")
    for t in sorted(times):
        print(f"- {t:.4f} s")

if __name__ == "__main__":
    test_benchmark_full_game()
