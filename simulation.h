#pragma once

#include "jass.h"

namespace simulation {

jass::suit_t best_trump(jass::hand_t declarer_hand);
jass::hand_t swap_trump_one(jass::hand_t hand, jass::suit_t trump_mask);
std::array<jass::hand_t, 4> swap_trump_many(std::array<jass::hand_t, 4> hands, jass::suit_t trump_mask);
void initialize_swap_maps();
}
