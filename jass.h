#pragma once

#include <string>
#include <array> // For std::array
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
extern const suit_t NOT_A_SUIT;

extern const int POINTS_TABLE[37];

void initialize_card_maps();
card_t c(const std::string& desc);
std::string d(card_t card_mask);
std::string hand_to_string(hand_t hand);

int get_trick_points(card_t card1, card_t card2, card_t card3, card_t card4);
int trick_winner(card_t card1, card_t card2, card_t card3, card_t card4);
hand_t get_playable_cards(suit_t led_suit, hand_t hand);

suit_t get_suit(card_t card);

// The four solveN functions have been replaced by a single templated function.
template <int TRICK_DEPTH>
int solve_trick(
    std::array<card_t, 4>& hands,
    int current_player,
    hand_t remaining_cards,
    int alpha,
    int beta,
    boost::unordered_flat_map<uint64_t, int>& transposition_table,
    suit_t trick_led_suite,
    int trick_points_so_far,
    card_t trick_winning_card,
    int trick_winner_player
);


uint64_t transposition_key(hand_t remaining_cards, int current_player, card_t trick_led_card, int trick_points_so_far, card_t trick_winning_card);

int get_stock_bonus(hand_t hand);

int solve_deal(std::array<hand_t, 4>& hands);

} // namespace jass
