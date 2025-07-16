#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <array> // For std::array
#include <tuple> // For std::tuple
#include <boost/unordered/unordered_flat_map.hpp>

namespace jass {

using card_t = uint64_t;
using hand_t = uint64_t;
using suit_t = uint64_t;

enum Suit {
    CLUBS,
    DIAMONDS,
    HEARTS,
    SPADES,
};

extern const suit_t C;
extern const suit_t D;
extern const suit_t H;
extern const suit_t S;
extern const suit_t F;

extern const int FLAG_EXACT;
extern const int FLAG_LOWER_BOUND;
extern const int FLAG_UPPER_BOUND;

extern const int LAST_TRICK_BONUS;
extern const card_t NOT_A_CARD;

extern const int POINTS_TABLE[37];

void initialize_card_maps();
card_t c(const std::string& desc);
std::string d(card_t card_mask);
std::string hand_to_string(hand_t hand);

int get_trick_points(card_t card1, card_t card2, card_t card3, card_t card4);
int trick_winner(card_t card1, card_t card2, card_t card3, card_t card4);
hand_t get_playable_cards(suit_t led_suit, hand_t hand);

suit_t get_suit(card_t card);


// Declare the four recursive solver functions
int solve0(std::array<uint64_t, 4>& cards_in_hand, int current_player, uint64_t remaining_cards, int alpha, int beta,
           boost::unordered_flat_map<uint64_t, int>& transposition_table,
           card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card, int trick_winner_idx_in_trick);
int solve1(uint64_t card1, std::array<uint64_t, 4>& cards_in_hand, int current_player, uint64_t remaining_cards,
           int alpha, int beta,
           boost::unordered_flat_map<uint64_t, int>& transposition_table,
           card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card, int trick_winner_idx_in_trick);
int solve2(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand, int current_player,
           uint64_t remaining_cards, int alpha, int beta,
           boost::unordered_flat_map<uint64_t, int>& transposition_table,
           card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card, int trick_winner_idx_in_trick);
int solve3(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           int current_player, uint64_t remaining_cards, int alpha, int beta,
           boost::unordered_flat_map<uint64_t, int>& transposition_table,
           card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card, int trick_winner_idx_in_trick);

uint64_t transposition_key(hand_t remaining_cards, int current_player, card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card);

int get_stock_bonus(hand_t hand);

int solve_deal(std::array<hand_t, 4>& hands);

} // namespace jass
