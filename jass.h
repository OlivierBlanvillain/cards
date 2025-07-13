#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <map>
#include <tuple>

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

int get_card_bit(card_t card);
void initialize_card_maps();
card_t c(const std::string& desc);
std::string d(card_t card_mask);

int get_trick_points(card_t card1, card_t card2, card_t card3, card_t card4);
int trick_winner(card_t card1, card_t card2, card_t card3, card_t card4);
hand_t get_playable_cards(card_t card1, hand_t hand);
int solve_deal(std::vector<hand_t>& hands);

} // namespace jass
