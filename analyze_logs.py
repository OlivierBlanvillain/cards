import os
from collections import defaultdict
import math

# T-scores for 95% Confidence Interval (two-tailed, alpha = 0.05)
# Source: Common t-distribution tables. For df > 100, use 1.96 (Z-score).
# For intermediate df, we'll use the closest lower df available in this table.
t_scores_95_ci = {
    1: 12.706,
    2: 4.303,
    3: 3.182,
    4: 2.776,
    5: 2.571,
    6: 2.447, # Interpolated/common value
    7: 2.365, # Interpolated/common value
    8: 2.306, # Interpolated/common value
    9: 2.262, # Interpolated/common value
    10: 2.228,
    11: 2.201, # Interpolated/common value
    12: 2.179, # Interpolated/common value
    13: 2.160, # Interpolated/common value
    14: 2.145, # Interpolated/common value
    15: 2.131, # Interpolated/common value
    16: 2.120, # Interpolated/common value
    17: 2.110, # Interpolated/common value
    18: 2.101, # Interpolated/common value
    19: 2.093, # Interpolated/common value
    20: 2.086,
    21: 2.080, # Interpolated/common value
    22: 2.074, # Interpolated/common value
    23: 2.069, # Interpolated/common value
    24: 2.064, # Interpolated/common value
    25: 2.060, # Interpolated/common value
    26: 2.056, # Interpolated/common value
    27: 2.052, # Interpolated/common value
    28: 2.048, # Interpolated/common value
    29: 2.045, # Interpolated/common value
    30: 2.042,
    35: 2.030, # Interpolated/common value
    40: 2.021, # Interpolated/common value
    45: 2.014, # Interpolated/common value
    50: 2.009, # Interpolated/common value
    60: 2.000,
    70: 1.994, # Interpolated/common value
    80: 1.990, # Interpolated/common value
    90: 1.987, # Interpolated/common value
    100: 1.984,
}

def get_t_score(df):
    if df <= 0:
        return float('nan')
    if df >= 100:
        return 1.96 # Approximates Z-score for large df
    
    # Find the largest df in our table that is less than or equal to the requested df
    for key in sorted(t_scores_95_ci.keys(), reverse=True):
        if df >= key:
            return t_scores_95_ci[key]
    return float('nan') # Should not happen for df > 0

def calculate_mean(data):
    if not data:
        return 0.0
    return sum(data) / len(data)

def calculate_std_dev(data):
    if len(data) < 2:
        return 0.0 # Standard deviation requires at least 2 data points
    mean = calculate_mean(data)
    variance = sum([(x - mean) ** 2 for x in data]) / (len(data) - 1)
    return math.sqrt(variance)

def analyze_logs(sims_dir):
    repartition_scores = defaultdict(list)

    for filename in os.listdir(sims_dir):
        filepath = os.path.join(sims_dir, filename)
        if os.path.isfile(filepath):
            with open(filepath, 'r') as f:
                for line in f:
                    parts = line.strip().split(',')
                    if len(parts) >= 5:
                        try:
                            p1_hand_bits = parts[0]
                            score = int(parts[4])

                            # Extract the first 9 bits as trump repartition
                            trump_repartition = p1_hand_bits[:9]
                            repartition_scores[trump_repartition].append(score)
                        except ValueError:
                            continue

    results = {}
    for repartition, scores in repartition_scores.items():
        if scores:
            mean_score = calculate_mean(scores)
            std_dev = calculate_std_dev(scores)
            count = len(scores)
            
            margin_of_error = float('nan')
            if count > 1:
                df = count - 1
                t_score = get_t_score(df)
                if not math.isnan(t_score):
                    se = std_dev / math.sqrt(count)
                    margin_of_error = t_score * se

            results[repartition] = {
                'mean': mean_score,
                'std_dev': std_dev,
                'count': count,
                'margin_of_error': margin_of_error
            }
    return results

if __name__ == "__main__":
    current_dir = os.path.dirname(os.path.abspath(__file__))
    sims_directory = os.path.join(current_dir, 'sims')
    
    analysis_results = analyze_logs(sims_directory)

    # Print CSV header
    print("Trump Repartition,Mean Score,95% CI")

    # Print data rows
    for repartition, data in sorted(analysis_results.items()):
        ci_str = "NaN" # Default for not enough data
        if not math.isnan(data['margin_of_error']):
            ci_str = f"±{data['margin_of_error']:.2f}"
        
        print(f"{repartition},{data['mean']:.2f},{ci_str}")