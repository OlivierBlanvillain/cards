#!venv/bin/python3

from jass import solve_deal, RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d
import timeit
import statistics
import math

def test_benchmark_full_game():
    hands = [
        c("6♠,10♠,A♥,10♥,9♥,7♥,10♦,J♦,10♣"),
        c("6♥,A♠,9♠,Q♥,J♥,8♥,8♦,A♣,7♣"),
        c("6♦,K♠,J♠,K♥,K♦,9♦,7♦,K♣,Q♣"),
        c("6♣,Q♠,8♠,7♠,A♦,Q♦,J♣,9♣,8♣"),
    ]

    def run_solver():
        solve_deal(hands)

    num_trials = 2
    times = []
    for _ in range(num_trials):
        times.append(timeit.timeit(run_solver, number=1))

    mean_time = statistics.mean(times)
    stdev_time = statistics.stdev(times)
    # For 10 trials, t-score for 95% confidence interval is 2.262 (for 29 degrees of freedom)
    confidence_interval = 2.262 * (stdev_time / math.sqrt(num_trials))

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
