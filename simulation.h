#pragma once

#include "jass.h"

namespace simulation {

jass::suit_t best_trump_quick_eval(jass::hand_t declarer_hand);
jass::hand_t swap_trump_one(jass::hand_t hand, jass::suit_t trump_mask);
std::array<jass::hand_t, 4> swap_trump_many(std::array<jass::hand_t, 4> hands, jass::suit_t trump_mask);
jass::card_t c(const std::string& desc);
std::string hand_to_string(jass::hand_t hand);
void print_stats(const std::string& name, const std::vector<int>& scores, int iterations);
void run(int iterations, jass::hand_t hand = 0);

}
