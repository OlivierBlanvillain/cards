#include "jass.h"
#include <iostream>
#include <vector>
#include <random>
#include <numeric>
#include <algorithm>
#include <map>
#include <chrono>
#include <cmath> // For std::round

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

std::vector<jass::hand_t> swap_trump(std::vector<jass::hand_t> hands, jass::hand_t trump_mask) {
    if (trump_mask == jass::S) {
        return hands;
    }
    std::vector<jass::hand_t> new_hands;
    for (jass::hand_t hand : hands) {
        jass::hand_t spades_cards = hand & jass::S;
        jass::hand_t trump_cards = hand & trump_mask;
        jass::hand_t stable_cards = hand & ~(trump_mask | jass::S);

        for (jass::card_t card : iter_bits(spades_cards)) {
            stable_cards |= S_TO_T[trump_mask][card];
        }
        for (jass::card_t card : iter_bits(trump_cards)) {
            stable_cards |= T_TO_S[trump_mask][card];
        }
        new_hands.push_back(stable_cards);
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

std::vector<jass::hand_t> shuffle_other_hands(jass::hand_t declarer_hand) {
    jass::hand_t remaining_cards_mask = ALL_CARDS ^ declarer_hand;
    std::vector<jass::card_t> cards_to_deal;
    for (jass::card_t card : CARD_LIST) {
        if (remaining_cards_mask & card) {
            cards_to_deal.push_back(card);
        }
    }
    std::shuffle(cards_to_deal.begin(), cards_to_deal.end(), gen);

    std::vector<jass::hand_t> hands;
    hands.push_back(declarer_hand);
    hands.push_back(0);
    hands.push_back(0);
    hands.push_back(0);

    for (int i = 0; i < 9; ++i) hands[1] |= cards_to_deal[i];
    for (int i = 9; i < 18; ++i) hands[2] |= cards_to_deal[i];
    for (int i = 18; i < 27; ++i) hands[3] |= cards_to_deal[i];

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
    std::vector<double> scores;
    double mean = 0.0;
    double std_dev = 0.0;
    bool isActive = true;

    void update_stats() {
        if (scores.empty()) return;
        double sum = std::accumulate(scores.begin(), scores.end(), 0.0);
        mean = sum / scores.size();
        if (scores.size() > 1) {
            double sq_sum = 0.0;
            for(double score : scores) sq_sum += (score - mean) * (score - mean);
            std_dev = std::sqrt(sq_sum / (scores.size() - 1));
        } else {
            std_dev = 0.0;
        }
    }
};

// This function runs ONE full experiment for a given trump suit
double run_one_experiment(jass::hand_t declarer_hand, jass::hand_t trump_suit_mask) {
    std::cout << "Running one experiment...\n" << std::endl;
    std::vector<jass::hand_t> hands = shuffle_other_hands(declarer_hand);
    hands = swap_trump(hands, trump_suit_mask);
    return static_cast<double>(jass::solve_deal(hands));
}

// Calculates the 95% Confidence Interval for the difference of two means
std::pair<double, double> get_ci_for_difference(const MoveResult& res1, const MoveResult& res2) {
    double n1 = res1.scores.size(), n2 = res2.scores.size();
    double mean1 = res1.mean, mean2 = res2.mean;
    double std1 = res1.std_dev, std2 = res2.std_dev;
    double diff_mean = mean1 - mean2;
    double se_diff = std::sqrt((std1 * std1 / n1) + (std2 * std2 / n2));
    double margin_of_error = 1.96 * se_diff; // Z-score for 95% CI
    return {diff_mean - margin_of_error, diff_mean + margin_of_error};
}

void find_best_trump() {
    const int N_MIN = 10;
    const int BATCH_SIZE = 5;
    const int MAX_N_PER_MOVE = 500;
    const double INDIFFERENCE_THRESHOLD = 0.1;

    jass::hand_t declarer_hand = shuffle_one_hand();
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

        std::cout << std::fixed << std::setprecision(2);
        std::cout << "N(" << best_move->name << ")=" << std::setw(3) << best_move->scores.size()
                  << " | Best: " << best_move->name << " (" << best_move->mean << ")"
                  << " | 2nd: " << second_best_move->name << " (" << second_best_move->mean << ")"
                  << " | 95% CI for Diff: [" << std::setw(6) << ci_main.first << ", " << std::setw(6) << ci_main.second << "]" << std::endl;

        if (ci_main.first > 0 && second_best_move->isActive) {
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

int main() {
    jass::initialize_card_maps();
    initialize_swap_maps();
    find_best_trump();
    return 0;
}
