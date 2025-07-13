#!venv/bin/python3

from jass import solve_deal, RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d
import timeit
import statistics
import math

def test_benchmark_full_game():
    hands = [
        c("6S,10S,AH,10H,9H,7H,10D,JD,10C"),
        c("6H,AS,9S,QH,JH,8H,8D,AC,7C"),
        c("6D,KS,JS,KH,KD,9D,7D,KC,QC"),
        c("6C,QS,8S,7S,AD,QD,JC,9C,8C"),
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
