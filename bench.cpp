#include "jass.h"
#include <iostream>
#include <vector>
#include <string>
#include <chrono>
#include <numeric>
#include <cmath>
#include <algorithm>

void run_benchmark_full_game() {
    std::array<jass::hand_t, 4> hands = {
        0b100100000000111000000000000011010010,
        0b011001000011000100010000011000000000,
        0b000010001000000001101101100000001000,
        0b000000110100000010000010000100100101,
        // 0b010111110000101000000000000000000001,
        // 0b100000001000000000001011011000100100,
        // 0b001000000011010001010000000101000010,
        // 0b000000000100000110100100100010011000,
        // 0b111010100010000001000001000000000010,
        // 0b000101010000001110011000100000000000,
        // 0b000000000100010000100110000001011001,
        // 0b000000001001100000000000011110100100,
        // jass::c("6S,10S,AH,10H,9H,7H,10D,JD,10C"),
        // jass::c("6H,AS,9S,QH,JH,8H,8D,AC,7C"),
        // jass::c("6D,KS,JS,KH,KD,9D,7D,KC,QC"),
        // jass::c("6C,QS,8S,7S,AD,QD,JC,9C,8C"),
    };

    int num_trials = 10;
    std::vector<double> times;
    times.reserve(num_trials);

    for (int i = 0; i < num_trials; ++i) {
        std::cout << i << std::endl;
        auto start = std::chrono::high_resolution_clock::now();
        jass::solve_deal(hands);
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = end - start;
        times.push_back(duration.count());
    }

    double sum_times = std::accumulate(times.begin(), times.end(), 0.0);
    double mean_time = sum_times / num_trials;

    double sq_sum = 0.0;
    for (double t : times) {
        sq_sum += (t - mean_time) * (t - mean_time);
    }
    double stdev_time = std::sqrt(sq_sum / (num_trials - 1));

    // For 2 trials, t-score for 95% confidence interval is 12.706 (for 1 degree of freedom)
    double confidence_interval = 12.706 * (stdev_time / std::sqrt(num_trials));

    std::cout << std::endl;
    std::cout << "|---------------------|---------------|" << std::endl;
    std::cout << "| Metric              | Value         |" << std::endl;
    std::cout << "|---------------------|---------------|" << std::endl;
    std::cout << "| Mean Time           | " << std::fixed << std::setprecision(4) << mean_time << " s      |" << std::endl;
    std::cout << "| Standard Deviation  | " << std::fixed << std::setprecision(4) << stdev_time << " s      |" << std::endl;
    std::cout << "| 95% Confidence Int. | \xc2\xb1" << std::fixed << std::setprecision(4) << confidence_interval << " s     |" << std::endl;
    std::cout << "|-------------------------------------|" << std::endl;
    std::cout << "\nIndividual experiment outputs (sorted):" << std::endl;
    std::sort(times.begin(), times.end());
    for (double t : times) {
        std::cout << "- " << std::fixed << std::setprecision(4) << t << " s" << std::endl;
    }
}

int main() {
    jass::initialize_card_maps();
    run_benchmark_full_game();
    return 0;
}
