#pragma once

#include <string>
#include <array> // For std::array
#include <boost/unordered/unordered_flat_map.hpp>

namespace jass {

using card_t = uint64_t;
using hand_t = uint64_t;
using suit_t = uint64_t;

constexpr hand_t C = 0b000000000000000000000000000111111111;
constexpr hand_t D = 0b000000000000000000111111111000000000;
constexpr hand_t H = 0b000000000111111111000000000000000000;
constexpr hand_t S = 0b111111111000000000000000000000000000;

constexpr int FLAG_EXACT = 1 << 10;
constexpr int FLAG_LOWER_BOUND = 1 << 11;
constexpr int FLAG_UPPER_BOUND = 1 << 12;

constexpr int LAST_TRICK_BONUS = 5;
constexpr card_t NOT_A_CARD = 0;
constexpr suit_t NOT_A_SUIT = 0;

constexpr card_t JACK_OF_TRUMP = (1ULL << 35);
constexpr card_t KING_OF_TRUMP = (1ULL << 32);
constexpr card_t QUEEN_OF_TRUMP = (1ULL << 31);

extern const int POINTS_TABLE[37];

hand_t get_playable_cards(suit_t led_suit, hand_t hand);

template <int TRICK_DEPTH, int CURRENT_PLAYER>
int solve_trick(
    std::array<card_t, 4>& hands,
    hand_t remaining_cards,
    int alpha,
    int beta,
    boost::unordered_flat_map<uint32_t, int>& transposition_table,
    suit_t trick_led_suit,
    int trick_points_so_far,
    card_t trick_winning_card,
    int trick_winner_player,
    long long& visited_nodes
);

int get_stock_bonus(hand_t hand);

std::pair<int, long long> solve_deal(std::array<hand_t, 4>& hands);

} // namespace jass
