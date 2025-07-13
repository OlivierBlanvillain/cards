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

void simulate_one() {
    jass::hand_t declarer_hand = shuffle_one_hand();
    std::cout << pretty_print_hand(declarer_hand) << std::endl;

    std::vector<jass::hand_t> trump_suits = {jass::C, jass::D, jass::H, jass::S};

    for (jass::hand_t trump_suit_mask : trump_suits) {
        std::vector<double> scores;
        for (int i = 0; i < 10; ++i) {
            std::vector<jass::hand_t> hands = shuffle_other_hands(declarer_hand);
            hands = swap_trump(hands, trump_suit_mask);

            // This part removes the lowest value card from each hand.
            // This is likely for simulating a card being played.
            for (size_t j = 0; j < hands.size(); ++j) {
                if (hands[j] > 0) {
                    jass::card_t lowest_card = hands[j] & (-hands[j]); // Get the lowest set bit
                    hands[j] ^= lowest_card; // Remove it
                }
            }

            int score = jass::solve_deal(hands);
            scores.push_back(static_cast<double>(score));
        }
        double sum_scores = std::accumulate(scores.begin(), scores.end(), 0.0);
        double mean = sum_scores / scores.size();

        char trump_repr;
        if (trump_suit_mask == jass::S) trump_repr = 'S';
        else if (trump_suit_mask == jass::D) trump_repr = 'D';
        else if (trump_suit_mask == jass::H) trump_repr = 'H';
        else if (trump_suit_mask == jass::C) trump_repr = 'C';
        else trump_repr = '?';

        std::cout << trump_repr << " gives EV=" << std::round(mean * 100.0) / 100.0 << "pts (after " << 10 << " simulations)" << std::endl;
    }
}

int main() {
    jass::initialize_card_maps();
    initialize_swap_maps(); // Initialize simulation-specific maps

    for (int i = 0; i < 10; ++i) {
        simulate_one();
    }

    return 0;
}
