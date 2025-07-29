#include "jass.h"
#include <iostream>
#include <vector>
#include <random>
#include <numeric>
#include <algorithm>
#include <map>
#include <chrono>
#include <cmath> // For std::round
#include <bitset>

const std::array<double, 512> quick_eval = {58.65, 58.47, 62.16, 64.44, 59.05, 59.99, 58.34, 65.64, 61.58, 63.75, 62.66, 69.67, 65.87, 70.95, 71.59, 79.71, 58.45, 59.11, 59.85, 68.66, 59.86, 67.25, 69.60, 76.30, 63.32, 72.57, 71.50, 79.46, 70.22, 79.51, 81.54, 85.58, 58.91, 63.92, 62.24, 70.58, 62.48, 70.21, 69.56, 76.24, 61.53, 72.83, 74.98, 81.88, 73.10, 83.76, 83.43, 88.73, 84.55, 93.38, 91.33, 100.26, 94.01, 101.01, 102.36, 106.15, 94.82, 103.73, 103.27, 109.81, 103.23, 110.41, 111.23, 113.56, 62.20, 72.08, 71.88, 82.25, 69.29, 80.16, 80.46, 86.05, 70.66, 79.94, 83.31, 90.36, 83.80, 90.44, 90.67, 96.63, 70.20, 81.01, 80.04, 87.55, 81.28, 88.39, 84.88, 91.04, 83.21, 91.23, 88.82, 94.79, 93.17, 96.46, 96.04, 99.19, 70.92, 81.83, 84.24, 89.77, 83.63, 89.47, 90.22, 93.55, 85.22, 92.17, 91.73, 96.20, 93.77, 97.13, 96.78, 102.85, 103.76, 111.70, 110.11, 116.16, 112.06, 115.60, 114.89, 120.16, 111.54, 118.92, 117.67, 124.40, 118.36, 124.58, 123.07, 126.95, 73.12, 81.74, 77.13, 88.65, 77.44, 88.67, 87.40, 96.50, 80.12, 88.58, 89.17, 98.26, 90.04, 99.49, 98.53, 103.85, 78.67, 87.25, 89.35, 93.18, 87.07, 94.05, 94.90, 100.54, 89.89, 98.31, 96.83, 106.10, 97.21, 102.37, 105.46, 108.76, 80.94, 91.99, 89.86, 97.57, 91.84, 95.81, 98.99, 102.30, 93.24, 99.55, 98.49, 104.05, 101.38, 103.70, 104.63, 110.54, 110.56, 118.05, 120.19, 121.07, 115.50, 123.99, 123.78, 127.93, 119.78, 126.75, 127.42, 130.64, 125.70, 128.90, 129.24, 135.78, 87.07, 99.14, 98.35, 103.65, 99.97, 106.07, 106.60, 108.98, 99.10, 105.92, 106.75, 113.46, 109.46, 110.20, 112.77, 116.36, 98.36, 105.33, 104.96, 108.14, 104.43, 107.49, 109.29, 112.63, 106.51, 108.89, 111.54, 115.13, 110.33, 115.04, 114.22, 118.50, 99.92, 106.39, 105.47, 112.37, 104.79, 110.44, 110.93, 113.63, 107.46, 110.07, 112.30, 116.04, 112.07, 116.12, 115.88, 120.40, 127.18, 131.04, 132.06, 136.13, 130.96, 135.97, 135.13, 138.76, 131.27, 134.92, 135.19, 140.76, 134.86, 139.85, 138.68, 140.77, 99.79, 103.61, 104.81, 107.40, 101.43, 108.33, 105.59, 115.10, 102.18, 111.30, 110.37, 119.32, 108.39, 118.53, 118.41, 125.11, 102.10, 107.12, 106.41, 114.93, 106.49, 116.74, 114.19, 121.79, 108.22, 117.38, 118.66, 124.07, 117.10, 124.62, 124.50, 131.30, 102.35, 110.35, 110.34, 115.70, 109.61, 119.79, 118.12, 122.61, 108.46, 120.35, 119.58, 125.16, 118.68, 125.98, 124.57, 133.65, 131.98, 139.99, 139.91, 144.58, 139.22, 145.12, 145.29, 151.41, 141.32, 146.02, 146.11, 151.69, 146.44, 151.03, 152.15, 159.47, 110.03, 118.07, 116.77, 125.00, 118.84, 124.19, 125.79, 129.82, 118.91, 127.81, 126.71, 132.77, 128.21, 132.40, 132.96, 138.12, 117.16, 125.76, 124.87, 129.57, 125.64, 129.26, 130.85, 135.39, 125.18, 131.83, 130.68, 135.03, 131.27, 135.22, 136.03, 140.70, 118.45, 127.16, 127.49, 131.17, 126.44, 132.11, 131.11, 135.79, 127.76, 133.33, 133.01, 135.49, 133.42, 136.99, 136.33, 140.91, 147.78, 152.21, 153.56, 157.17, 153.24, 155.97, 156.77, 161.00, 152.72, 156.32, 156.84, 161.59, 156.69, 160.21, 161.29, 168.11, 116.94, 125.31, 125.92, 133.42, 124.34, 132.86, 132.75, 139.10, 125.74, 133.43, 133.21, 139.25, 135.05, 139.68, 140.53, 143.59, 124.06, 133.35, 131.43, 139.82, 131.92, 138.65, 138.64, 141.62, 132.98, 138.50, 138.67, 142.29, 138.62, 140.92, 142.78, 145.91, 125.90, 134.75, 134.19, 139.08, 135.32, 139.18, 138.72, 142.20, 135.81, 140.66, 140.35, 142.89, 139.46, 143.32, 143.03, 145.28, 154.57, 159.39, 158.56, 163.51, 159.84, 162.58, 161.67, 166.07, 158.88, 162.94, 162.61, 166.03, 162.60, 165.86, 165.94, 168.76, 133.60, 140.39, 142.05, 144.49, 140.40, 145.74, 144.76, 145.98, 141.70, 146.41, 145.53, 146.84, 145.10, 146.51, 147.19, 148.28, 140.12, 142.99, 144.15, 144.81, 143.55, 144.70, 144.39, 146.07, 142.27, 145.52, 144.47, 146.19, 144.98, 146.29, 145.95, 148.96, 139.76, 144.57, 143.45, 146.17, 143.88, 145.40, 145.49, 147.04, 143.45, 145.11, 145.62, 146.04, 144.73, 146.24, 145.74, 148.90, 162.49, 165.02, 165.50, 166.74, 165.38, 166.76, 165.73, 169.50, 161.87, 165.98, 166.74, 168.59, 163.66, 168.52, 166.11, 177.00};

jass::suit_t best_trump(jass::hand_t declarer_hand) {
    return jass::S;
}

// Constants
const std::vector<jass::card_t> CARD_LIST = []() {
    std::vector<jass::card_t> cards;
    for (int i = 0; i < 36; ++i) {
        cards.push_back(1ULL << i);
    }
    return cards;
}();
const jass::hand_t ALL_CARDS = std::accumulate(CARD_LIST.begin(), CARD_LIST.end(), (jass::hand_t)0);

std::map<jass::hand_t, int> SUIT_BIT_START;

// Helper to iterate over set bits in a hand
std::vector<jass::card_t> iter_bits(jass::hand_t hand) {
    std::vector<jass::card_t> cards;
    while (hand > 0) {
        jass::card_t card = hand & (-hand); // Get the lowest set bit
        cards.push_back(card);
        hand ^= card; // Clear the lowest set bit
    }
    return cards;
}

// Global random device and generator
std::random_device rd;
std::mt19937 gen(rd());

// Forward declarations for swap maps
std::map<jass::hand_t, std::map<jass::card_t, jass::card_t>> T_TO_S;
std::map<jass::hand_t, std::map<jass::card_t, jass::card_t>> S_TO_T;

void initialize_swap_maps() {
    SUIT_BIT_START[jass::CLUBS] = 0;
    SUIT_BIT_START[jass::DIAMONDS] = 9;
    SUIT_BIT_START[jass::HEARTS] = 18;
    SUIT_BIT_START[jass::SPADES] = 27;

    std::map<int, int> ti_to_si = {
        {0, 0}, {1, 1}, {2, 2}, {3, 7}, {4, 3}, {5, 8}, {6, 4}, {7, 5}, {8, 6}
    };

    for (auto const& [t_suit_enum, t_start_bit] : SUIT_BIT_START) {
        jass::hand_t t_suit_mask;
        if (t_suit_enum == jass::CLUBS) t_suit_mask = jass::C;
        else if (t_suit_enum == jass::DIAMONDS) t_suit_mask = jass::D;
        else if (t_suit_enum == jass::HEARTS) t_suit_mask = jass::H;
        else if (t_suit_enum == jass::SPADES) t_suit_mask = jass::S;
        else continue; // Should not happen

        T_TO_S[t_suit_mask] = {};
        S_TO_T[t_suit_mask] = {};

        int s_start_bit = SUIT_BIT_START[jass::SPADES];

        for (auto const& [ti, si] : ti_to_si) {
            jass::card_t t_card = 1ULL << (t_start_bit + ti);
            jass::card_t s_card = 1ULL << (s_start_bit + si);
            T_TO_S[t_suit_mask][t_card] = s_card;
            S_TO_T[t_suit_mask][s_card] = t_card;
        }
    }
}

std::array<jass::hand_t, 4> swap_trump(std::array<jass::hand_t, 4> hands, jass::hand_t trump_mask) {
    if (trump_mask == jass::S) {
        return hands;
    }
    std::array<jass::hand_t, 4> new_hands;
    for (size_t i = 0; i < hands.size(); ++i) {
        jass::hand_t hand = hands[i];
        jass::hand_t spades_cards = hand & jass::S;
        jass::hand_t trump_cards = hand & trump_mask;
        jass::hand_t stable_cards = hand & ~(trump_mask | jass::S);

        for (jass::card_t card : iter_bits(spades_cards)) {
            stable_cards |= S_TO_T[trump_mask][card];
        }
        for (jass::card_t card : iter_bits(trump_cards)) {
            stable_cards |= T_TO_S[trump_mask][card];
        }
        new_hands[i] = stable_cards;
    }
    return new_hands;
}

jass::hand_t shuffle_one_hand() {
    std::vector<jass::card_t> shuffled_cards = CARD_LIST;
    std::shuffle(shuffled_cards.begin(), shuffled_cards.end(), gen);
    jass::hand_t chosen_hand = 0;
    for (int i = 0; i < 9; ++i) {
        chosen_hand |= shuffled_cards[i];
    }
    return chosen_hand;
}

std::array<jass::hand_t, 4> shuffle_other_hands(jass::hand_t bidder_hand, int bidder_idx) {
    jass::hand_t remaining_cards_mask = ALL_CARDS ^ bidder_hand;
    std::vector<jass::card_t> cards_to_deal;
    for (jass::card_t card : CARD_LIST) {
        if (remaining_cards_mask & card) {
            cards_to_deal.push_back(card);
        }
    }
    std::shuffle(cards_to_deal.begin(), cards_to_deal.end(), gen);

    std::array<jass::hand_t, 4> hands;
    hands.fill(0); // Initialize all hands to 0
    hands[bidder_idx] = bidder_hand;

    int current_card_idx = 0;
    for (int i = 0; i < 4; ++i) {
        if (i == bidder_idx) continue;
        for (int j = 0; j < 9; ++j) {
            hands[i] |= cards_to_deal[current_card_idx++];
        }
    }
    return hands;
}

std::string pretty_print_hand(jass::hand_t hand) {
    std::string result = "";
    std::vector<jass::card_t> cards = iter_bits(hand);
    std::reverse(cards.begin(), cards.end()); // To match Python's reversed iteration
    for (size_t i = 0; i < cards.size(); ++i) {
        result += jass::d(cards[i]);
        if (i < cards.size() - 1) {
            result += ", ";
        }
    }
    return result;
}


// A structure to hold the results for each potential move (trump suit)
struct MoveResult {
    char name;
    jass::hand_t move_mask;
    std::vector<double> scores{}; // Initialize scores
    double mean = 0.0;
    double std_dev = 0.0;
    bool isActive = true;

    void update_stats() {
        if (scores.empty()) return;
        double sum = std::accumulate(scores.begin(), scores.end(), 0.0);
        mean = sum / static_cast<double>(scores.size());
        if (scores.size() > 1) {
            double sq_sum = 0.0;
            for(double score : scores) sq_sum += (score - mean) * (score - mean);
            std_dev = std::sqrt(sq_sum / (static_cast<double>(scores.size()) - 1.0));
        } else {
            std_dev = 0.0;
        }
    }
};

double run_one_experiment(jass::hand_t declarer_hand, jass::hand_t trump_suit_mask) {
    std::array<jass::hand_t, 4> hands = shuffle_other_hands(declarer_hand, 0);
    hands = swap_trump(hands, trump_suit_mask);
    return static_cast<double>(jass::solve_deal(hands).first);
}

// Calculates the 95% Confidence Interval for a single mean
std::pair<double, double> get_ci_for_mean(const MoveResult& res) {
    if (res.scores.size() < 2) {
        return {0.0, 0.0}; // Not enough data for a meaningful CI
    }
    double n = static_cast<double>(res.scores.size());
    double mean = res.mean;
    double se = res.std_dev / std::sqrt(n);
    double margin_of_error = 1.96 * se; // Z-score for 95% CI
    return {mean - margin_of_error, mean + margin_of_error};
}

// Calculates the 95% Confidence Interval for the difference of two means
std::pair<double, double> get_ci_for_difference(const MoveResult& res1, const MoveResult& res2) {
    double n1 = static_cast<double>(res1.scores.size()), n2 = static_cast<double>(res2.scores.size());
    double mean1 = res1.mean, mean2 = res2.mean;
    double std1 = res1.std_dev, std2 = res2.std_dev;
    double diff_mean = mean1 - mean2;
    double se_diff = std::sqrt((std1 * std1 / n1) + (std2 * std2 / n2));
    double margin_of_error = 1.96 * se_diff; // Z-score for 95% CI
    return {diff_mean - margin_of_error, diff_mean + margin_of_error};
}

void find_best_trump() {
    const int N_MIN = 3;
    const int BATCH_SIZE = 1;
    const int MAX_N_PER_MOVE = 50;
    const double INDIFFERENCE_THRESHOLD = 0.1;

    jass::hand_t declarer_hand = shuffle_one_hand();
    // jass::hand_t declarer_hand = jass::c("JS,9S,AH,10H,KH,QH,7D,8D,7C");
    std::cout << "Declarer Hand: " << pretty_print_hand(declarer_hand) << std::endl;
    std::cout << "Finding best trump suit...\n" << std::endl;

    std::vector<MoveResult> results = {
        {'S', jass::S}, {'H', jass::H}, {'D', jass::D}, {'C', jass::C}
    };

    for (int i = 0; i < N_MIN; ++i) {
        for (auto& res : results) {
            res.scores.push_back(run_one_experiment(declarer_hand, res.move_mask));
        }
    }

    while (true) {
        // --- 1. Update stats and identify active contenders ---
        std::vector<MoveResult*> active_contenders;
        for (auto& res : results) {
            if (res.isActive) {
                res.update_stats();
                active_contenders.push_back(&res);
            }
        }

        // --- 2. Check for a single winner ---
        if (active_contenders.size() <= 1) {
            std::cout << "\n--- Conclusion: Found a Single Best Move! ---" << std::endl;
            if (!active_contenders.empty()) {
                std::cout << "Trump " << active_contenders[0]->name << " is the winner with EV: " << active_contenders[0]->mean << std::endl;
            } else {
                std::cout << "Error: No active contenders left." << std::endl;
            }
            return;
        }

        // --- 3. Sort active contenders by mean score ---
        std::sort(active_contenders.begin(), active_contenders.end(), [](const auto* a, const auto* b) {
            return a->mean > b->mean;
        });

        MoveResult* best_move = active_contenders[0];
        MoveResult* second_best_move = active_contenders[1];

        // --- 4. Pruning Phase: Compare best against all other active challengers ---
        for (size_t i = 1; i < active_contenders.size(); ++i) {
            MoveResult* challenger = active_contenders[i];
            auto ci_prune = get_ci_for_difference(*best_move, *challenger);
            if (ci_prune.first > 0) {
                challenger->isActive = false;
                std::cout << "    -> Pruning move " << challenger->name << " (EV " << challenger->mean
                          << "). Confident it's worse than " << best_move->name << " (EV " << best_move->mean << ")." << std::endl;
            }
        }

        // --- 5. Status Report & Main Stopping Conditions ---
        auto ci_main = get_ci_for_difference(*best_move, *second_best_move);
        auto ci_best = get_ci_for_mean(*best_move);
        auto ci_second = get_ci_for_mean(*second_best_move);

        std::cout << std::fixed << std::setprecision(2);
        std::cout << "N(" << best_move->name << ")=" << std::setw(4) << best_move->scores.size()
                  << " | Best: " << best_move->name << " (" << best_move->mean << " CI: [" << ci_best.first << ", " << ci_best.second << "])"
                  << " | 2nd: " << second_best_move->name << " (" << second_best_move->mean << " CI: [" << ci_second.first << ", " << ci_second.second << "])"
                  << " | 95% CI for Diff: [" << std::setw(6) << ci_main.first << ", " << std::setw(6) << ci_main.second << "]" << std::endl;

        if (ci_main.first > 0 && second_best_move->isActive) {
            throw std::invalid_argument("unreachable");
            std::cout << "\n--- Conclusion: Found a Confident Winner! ---" << std::endl;
            std::cout << "Trump " << best_move->name << " is significantly better than " << second_best_move->name << "." << std::endl;
            return;
        }

        if ((ci_main.second - ci_main.first) < INDIFFERENCE_THRESHOLD) {
            std::cout << "\n--- Conclusion: Moves are Practically Equivalent ---" << std::endl;
            std::cout << "Difference between " << best_move->name << " and " << second_best_move->name
                      << " is smaller than the threshold of " << INDIFFERENCE_THRESHOLD << " pts." << std::endl;
            return;
        }

        // --- 6. Check max simulations for the leading contender ---
        if (best_move->scores.size() >= MAX_N_PER_MOVE) {
            std::cout << "\n--- Conclusion: Reached Max Simulations (" << MAX_N_PER_MOVE << ") ---" << std::endl;
            std::cout << "Result is inconclusive for top contenders." << std::endl;
            return;
        }

        // --- 7. Run next batch ONLY for active moves ---
        for (auto& res : results) {
            if (res.isActive) {
                for (int i = 0; i < BATCH_SIZE; ++i) {
                    res.scores.push_back(run_one_experiment(declarer_hand, res.move_mask));
                }
            }
        }
    }
}

std::vector<jass::hand_t> generate_all_trump_hands() {
    std::vector<jass::card_t> trump_cards;
    std::vector<jass::card_t> non_trump_cards;
    jass::hand_t full_deck = jass::C | jass::D | jass::H | jass::S;

    for (int i = 0; i < 36; ++i) {
        jass::card_t current_card = 1ULL << i;
        if ((current_card & full_deck) == 0) continue; // Skip unused bits if any
        if (current_card & jass::S) {
            trump_cards.push_back(current_card);
        } else {
            non_trump_cards.push_back(current_card);
        }
    }
    std::random_device random_device;
    std::mt19937 g(random_device());

    std::vector<jass::hand_t> generated_hands;
    generated_hands.reserve(512);

    for (int i = 0; i < 512; ++i) {
        jass::hand_t current_hand = 0;
        int num_trumps = 0;
        for (int j = 0; j < 9; ++j) {
            if ((i >> j) & 1) {
                current_hand |= trump_cards[j];
                num_trumps++;
            }
        }
        int num_non_trumps_needed = 9 - num_trumps;
        std::shuffle(non_trump_cards.begin(), non_trump_cards.end(), g);
        for (int k = 0; k < num_non_trumps_needed; ++k) {
            current_hand |= non_trump_cards[k];
        }
        generated_hands.push_back(current_hand);
    }

    return generated_hands;
}


int sampling_all_trump_hands() {
    std::vector<jass::hand_t> samples = generate_all_trump_hands();
    std::shuffle(samples.begin(), samples.end(), gen);
    for (jass::hand_t bidder_hand : samples) {
        std::array<jass::hand_t, 4> hands = shuffle_other_hands(bidder_hand, 0);
        std::bitset<36> p1_bits(hands[0]);
        std::bitset<36> p2_bits(hands[1]);
        std::bitset<36> p3_bits(hands[2]);
        std::bitset<36> p4_bits(hands[3]);

        std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
        int score = jass::solve_deal(hands).first;
        std::chrono::steady_clock::time_point stop = std::chrono::steady_clock::now();

        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count();
        std::cout << p1_bits << "," << p2_bits << "," << p3_bits << "," << p4_bits << "," << score << "," << elapsed << "ms" << std::endl;
    }

    // jass::initialize_card_maps();
    // initialize_swap_maps();
    // find_best_trump();
    return 0;
}
