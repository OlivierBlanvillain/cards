from cards import double_dummy_solver0, double_dummy_solver1, double_dummy_solver2, double_dummy_solver3
import timeit
import statistics
import math

from test_utils import RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d

def test_benchmark_full_game():
    hands = (
        c("J♠,9♠,A♠,10♠,K♠,Q♠,8♠,7♠"),
        c("A♥,10♥,K♥,Q♥,J♥,9♥,8♥,7♥"),
        c("A♦,10♦,K♦,Q♦,J♦,9♦,8♦,7♦"),
        c("A♣,10♣,K♣,Q♣,J♣,9♣,8♣,7♣"),
    )

    def run_solver():
        double_dummy_solver0(tuple(hands), 0, use_alpha_beta=True, alpha=-999, beta=999)

    num_trials = 30
    times = []
    for _ in range(num_trials):
        double_dummy_solver0.cache_clear()
        double_dummy_solver1.cache_clear()
        double_dummy_solver2.cache_clear()
        double_dummy_solver3.cache_clear()
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
