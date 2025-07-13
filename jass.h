#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <array> // For std::array
#include <tuple> // For std::tuple
#include "ankerl/unordered_dense.h" // For unordered_dense::map

namespace jass {

using card_t = uint64_t;
using hand_t = uint64_t;

enum Suit {
    CLUBS,
    DIAMONDS,
    HEARTS,
    SPADES,
};

extern const hand_t C;
extern const hand_t D;
extern const hand_t H;
extern const hand_t S;
extern const hand_t F;

extern const int FLAG_EXACT;
extern const int FLAG_LOWER_BOUND;
extern const int FLAG_UPPER_BOUND;

extern const int LAST_TRICK_BONUS;
extern const card_t NOT_A_CARD;

extern const int POINTS_TABLE[37];

void initialize_card_maps();
card_t c(const std::string& desc);
std::string d(card_t card_mask);

int get_trick_points(card_t card1, card_t card2, card_t card3, card_t card4);
int trick_winner(card_t card1, card_t card2, card_t card3, card_t card4);
hand_t get_playable_cards(card_t card1, hand_t hand);

// Transposition Table Entry
struct TranspositionTableEntry {
    int score;
    int flag;
};

// State types for different recursion depths
using State0 = std::tuple<uint64_t, int>; // remaining_cards, current_player
using State1 = std::tuple<uint64_t, uint64_t, int>; // card1, remaining_cards, current_player
using State2 = std::tuple<uint64_t, uint64_t, uint64_t, int>; // card1, card2, remaining_cards, current_player
using State3 = std::tuple<uint64_t, uint64_t, uint64_t, uint64_t, int>; // card1, card2, card3, remaining_cards, current_player

// Declare the four recursive solver functions
int solve0(std::array<uint64_t, 4>& cards_in_hand, int current_player, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3);
int solve1(uint64_t card1, std::array<uint64_t, 4>& cards_in_hand, int current_player, uint64_t remaining_cards,
           int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3);
int solve2(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand, int current_player,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3);
int solve3(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           int current_player, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3);

// Main entry point for solving a deal
int solve_deal(std::array<hand_t, 4>& hands);

} // namespace jass
