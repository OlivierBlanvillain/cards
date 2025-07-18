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

const int TRUMP_EVAL_TABLE[37] = {
    -1, // unused (index 0)
    // 6, 7, 8, 9, 10, J, Q, K, As
    13+0, 13+0, 13+0, 13+14, 13+10, 13+20, 13+3, 13+4, 13+11, // Clubs (bit_length 1-9)
    13+0, 13+0, 13+0, 13+14, 13+10, 13+20, 13+3, 13+4, 13+11, // Diamonds (bit_length 10-18)
    13+0, 13+0, 13+0, 13+14, 13+10, 13+20, 13+3, 13+4, 13+11, // Hearts (bit_length 19-27)
    // 6, 7, 8, 10, Q, K, As, 9,  J
    13+0, 13+0, 13+0, 13+10, 13+3, 13+4, 13+11, 13+14, 13+20, // Spades (bit_length 28-36)
};

jass::hand_t best_trump(jass::hand_t declarer_hand) {
    int c_count = 0;
    int d_count = 0;
    int h_count = 0;
    int s_count = 0;
    while (declarer_hand) {
        jass::card_t card = declarer_hand & -declarer_hand;
        declarer_hand ^= card;
        int eval = TRUMP_EVAL_TABLE[std::bit_width(card)];
        if (card & jass::C) {
            c_count += eval;
        } else if (card & jass::D) {
            d_count += eval;
        } else if (card & jass::H) {
            h_count += eval;
        } else {
            s_count += eval;
        }
    }
    if (c_count >= d_count && c_count >= h_count && c_count >= s_count) {
        return jass::C;
    }
    if (d_count >= h_count && d_count >= s_count) {
        return jass::D;
    }
    if (h_count >= s_count) {
        return jass::H;
    }
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
    return static_cast<double>(jass::solve_deal(hands));
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


int main() {
    std::vector<jass::hand_t> samples = generate_all_trump_hands();
    std::shuffle(samples.begin(), samples.end(), gen);
    for (jass::hand_t bidder_hand : samples) {
        std::array<jass::hand_t, 4> hands = shuffle_other_hands(bidder_hand, 0);
        std::bitset<36> p1_bits(hands[0]);
        std::bitset<36> p2_bits(hands[1]);
        std::bitset<36> p3_bits(hands[2]);
        std::bitset<36> p4_bits(hands[3]);
        std::cout << p1_bits << "," << p2_bits << "," << p3_bits << "," << p4_bits << std::flush;

        std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
        int score = jass::solve_deal(hands);
        std::chrono::steady_clock::time_point stop = std::chrono::steady_clock::now();

        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count();
        std::cout << "," << score << "," << elapsed << "ms" << std::endl;
    }

    // jass::initialize_card_maps();
    // initialize_swap_maps();
    // find_best_trump();
    return 0;
}
