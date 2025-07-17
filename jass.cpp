#include "jass.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <map> // For std::map
#include <boost/unordered/unordered_flat_map.hpp>
#include <bit> // For std::countl_zero
#include <string>
#include <sstream>
#include <chrono>
#include <ctime>
#include <iomanip>

#include <fstream>

namespace jass {

const hand_t C = 0b000000000000000000000000000111111111;
const hand_t D = 0b000000000000000000111111111000000000;
const hand_t H = 0b000000000111111111000000000000000000;
const hand_t S = 0b111111111000000000000000000000000000;

const int FLAG_EXACT = 1 << 10;
const int FLAG_LOWER_BOUND = 1 << 11;
const int FLAG_UPPER_BOUND = 1 << 12;

const int LAST_TRICK_BONUS = 5;
const card_t NOT_A_CARD = 0;
const suit_t NOT_A_SUIT = 0;

const card_t JACK_OF_TRUMP = (1ULL << 35);
const hand_t F = S ^ JACK_OF_TRUMP;

// 1-indexed by bit_length (0-36)
const int POINTS_TABLE[37] = {
    -1, // unused (index 0)
    // 6, 7, 8, 9, 10, J, Q, K, As
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Clubs (bit_length 1-9)
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Diamonds (bit_length 10-18)
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Hearts (bit_length 19-27)
    // 6, 7, 8, 10, Q, K, As, 9,  J
    0, 0, 0, 10, 3, 4, 11, 14, 20, // Spades (bit_length 28-36)
};

// 1-indexed by bit_length (0-36)
static const hand_t SUIT_TABLE[37] = {
    0, // unused (index 0)
    C, C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H, H,
    S, S, S, S, S, S, S, S, S,
};

// 1-indexed by bit_length (0-36)
static const suit_t FOLLOW_TABLE[37] = {
    0, // unused (index 0)
    C, C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H, H,
    F, F, F, F, F, F, F, F, F,
};

std::map<std::pair<Suit, std::string>, int> CARD_TO_BIT;
std::map<int, std::tuple<Suit, std::string, char>> BIT_TO_CARD;

suit_t get_suit(card_t card) {
    return SUIT_TABLE[std::bit_width(card)];
}

void initialize_card_maps() {
    const char* RANKS_TRUMP[] = {"J", "9", "A", "K", "Q", "10", "8", "7", "6"};
    const char* RANKS_PLAIN[] = {"A", "K", "Q", "J", "10", "9", "8", "7", "6"};

    int bit = 35; // 0-indexed bit position
    for (auto const& [suit_val, suit_name] : std::vector<std::pair<suit_t, Suit>>{{S, SPADES}, {H, HEARTS}, {D, DIAMONDS}, {C, CLUBS}}) {
        const char** ranks = (suit_val == S) ? RANKS_TRUMP : RANKS_PLAIN;
        int num_ranks = 9;
        for (int i = 0; i < num_ranks; ++i) {
            CARD_TO_BIT[{suit_name, ranks[i]}] = bit;
            BIT_TO_CARD[bit] = {suit_name, ranks[i], "CDHS"[static_cast<int>(suit_name)]};
            bit--;
        }
    }
}

card_t c(const std::string& desc) {
    card_t total = 0;
    if (desc.empty()) return total;
    size_t start = 0;
    size_t end = desc.find(',');
    while (end != std::string::npos) {
        std::string token = desc.substr(start, end - start);
        std::string rank = token.substr(0, token.length() - 1);
        char suit_char = token.back();
        Suit suit;
        if (suit_char == 'C') suit = CLUBS;
        else if (suit_char == 'D') suit = DIAMONDS;
        else if (suit_char == 'H') suit = HEARTS;
        else suit = SPADES;
        total |= 1ULL << CARD_TO_BIT.at({suit, rank});
        start = end + 1;
        end = desc.find(',', start);
    }
    std::string token = desc.substr(start);
    std::string rank = token.substr(0, token.length() - 1);
    char suit_char = token.back();
    Suit suit;
    if (suit_char == 'C') suit = CLUBS;
    else if (suit_char == 'D') suit = DIAMONDS;
    else if (suit_char == 'H') suit = HEARTS;
    else suit = SPADES;
    total |= 1ULL << CARD_TO_BIT.at({suit, rank});
    return total;
}

std::string d(card_t card_mask) {
    if (card_mask == 0) return "";
    int bit_pos = std::bit_width(card_mask); // This is 1-indexed bit_length (0-36)
    if (bit_pos == 0) return ""; // Handle NOT_A_CARD case
    auto const& [suit, rank, suit_char] = BIT_TO_CARD.at(bit_pos - 1); // Convert to 0-indexed for BIT_TO_CARD
    return rank + suit_char;
}

std::string hand_to_string(hand_t hand) {
    std::string s = "";
    for (int i = 0; i < 36; ++i) {
        if ((hand >> i) & 1) {
            s += d(1ULL << i) + ",";
        }
    }
    if (!s.empty()) {
        s.pop_back(); // Remove trailing comma
    }
    return s;
}

int get_trick_points(card_t card1, card_t card2, card_t card3, card_t card4) {
    int points = 0;
    if (card1 != NOT_A_CARD) points += POINTS_TABLE[std::bit_width(card1)];
    if (card2 != NOT_A_CARD) points += POINTS_TABLE[std::bit_width(card2)];
    if (card3 != NOT_A_CARD) points += POINTS_TABLE[std::bit_width(card3)];
    if (card4 != NOT_A_CARD) points += POINTS_TABLE[std::bit_width(card4)];
    return points;
}

int trick_winner(card_t card1, card_t card2, card_t card3, card_t card4) {
    hand_t led_suit = get_suit(card1);
    suit_t led_mask = S | led_suit;
    card2 &= led_mask;
    card3 &= led_mask;
    card4 &= led_mask;
    if (card1 >= card2 && card1 >= card3 && card1 >= card4) return 0;
    if (card2 >= card3 && card2 >= card4) return 1;
    if (card3 >= card4) return 2;
    return 3;
}

hand_t get_playable_cards(suit_t led_suit, hand_t hand) {
    hand_t follow = hand & led_suit;
    if (follow == JACK_OF_TRUMP) {
        return hand;
    }
    if (follow) {
        return follow | (hand & JACK_OF_TRUMP);
    }
    return hand;
}

int solve0(
    std::array<card_t, 4>& hands,
    int current_player,
    hand_t remaining_cards,
    int alpha,
    int beta,
    boost::unordered_flat_map<uint64_t, int>& transposition_table
) {
    if (remaining_cards == 0) {
        return 0;
    }

    int initial_alpha = alpha;
    uint64_t state_key = transposition_key(remaining_cards, current_player, NOT_A_SUIT, 0, NOT_A_CARD);
    auto it = transposition_table.find(state_key);
    if (it != transposition_table.end()) {
        int entry = it->second;
        if (entry & FLAG_EXACT) {
            return entry ^ FLAG_EXACT;
        } else if (entry & FLAG_LOWER_BOUND) {
            int score = entry ^ FLAG_LOWER_BOUND;
            alpha = std::max(alpha, score);
            if (alpha >= beta) return score;
        } else if (entry & FLAG_UPPER_BOUND) {
            int score = entry ^ FLAG_UPPER_BOUND;
            beta = std::min(beta, score);
            if (alpha >= beta) return score;
        }
    }

    bool is_maximizing_player = (current_player % 2 == 0);
    int best_score = is_maximizing_player ? -999 : 999;

    hand_t playable_cards = hands[current_player];
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        hands[current_player] ^= card;
        int current_move_value = solve1(
            hands,
            (current_player + 1) % 4,
            remaining_cards ^ card,
            alpha,
            beta,
            transposition_table,
            get_suit(card),
            POINTS_TABLE[std::bit_width(card)],
            card,
            current_player
        );
        hands[current_player] ^= card;

        if (is_maximizing_player) {
            best_score = std::max(best_score, current_move_value);
            alpha = std::max(alpha, best_score);
        } else {
            best_score = std::min(best_score, current_move_value);
            beta = std::min(beta, best_score);
        }

        if (beta <= alpha) {
            break;
        }
    }

    int flag;
    if (best_score <= initial_alpha) {
        flag = FLAG_UPPER_BOUND;
    } else if (best_score >= beta) {
        flag = FLAG_LOWER_BOUND;
    } else {
        flag = FLAG_EXACT;
    }
    transposition_table[state_key] = best_score | flag;

    return best_score;
}

int solve1(
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
){
    int initial_alpha = alpha;
    uint64_t state_key = transposition_key(remaining_cards, current_player, trick_led_suite, trick_points_so_far, trick_winning_card);
    auto it = transposition_table.find(state_key);
    if (it != transposition_table.end()) {
        int entry = it->second;
        if (entry & FLAG_EXACT) {
            return entry ^ FLAG_EXACT;
        } else if (entry & FLAG_LOWER_BOUND) {
            int score = entry ^ FLAG_LOWER_BOUND;
            alpha = std::max(alpha, score);
            if (alpha >= beta) return score;
        } else if (entry & FLAG_UPPER_BOUND) {
            int score = entry ^ FLAG_UPPER_BOUND;
            beta = std::min(beta, score);
            if (alpha >= beta) return score;
        }
    }

    bool is_maximizing_player = (current_player % 2 == 0);
    int best_score = is_maximizing_player ? -999 : 999;

    hand_t playable_cards = get_playable_cards(trick_led_suite, hands[current_player]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int new_points_so_far = trick_points_so_far + POINTS_TABLE[std::bit_width(card)];
        card_t new_winning_card = trick_winning_card;
        int new_winner_player = trick_winner_player;
        if ((card & (trick_led_suite | S)) > trick_winning_card) {
            new_winning_card = card;
            new_winner_player = current_player;
        }

        hands[current_player] ^= card;
        int current_move_value = solve2(
            hands,
            (current_player + 1) % 4,
            remaining_cards ^ card,
            alpha,
            beta,
            transposition_table,
            trick_led_suite,
            new_points_so_far,
            new_winning_card,
            new_winner_player
        );
        hands[current_player] ^= card;

        if (is_maximizing_player) {
            best_score = std::max(best_score, current_move_value);
            alpha = std::max(alpha, best_score);
        } else {
            best_score = std::min(best_score, current_move_value);
            beta = std::min(beta, best_score);
        }

        if (beta <= alpha) {
            break;
        }
    }

    int flag;
    if (best_score <= initial_alpha) {
        flag = FLAG_UPPER_BOUND;
    } else if (best_score >= beta) {
        flag = FLAG_LOWER_BOUND;
    } else {
        flag = FLAG_EXACT;
    }
    transposition_table[state_key] = best_score | flag;

    return best_score;
}

int solve2(
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
) {
    int initial_alpha = alpha;
    uint64_t state_key = transposition_key(remaining_cards, current_player, trick_led_suite, trick_points_so_far, trick_winning_card);
    auto it = transposition_table.find(state_key);
    if (it != transposition_table.end()) {
        int entry = it->second;
        if (entry & FLAG_EXACT) {
            return entry ^ FLAG_EXACT;
        } else if (entry & FLAG_LOWER_BOUND) {
            int score = entry ^ FLAG_LOWER_BOUND;
            alpha = std::max(alpha, score);
            if (alpha >= beta) return score;
        } else if (entry & FLAG_UPPER_BOUND) {
            int score = entry ^ FLAG_UPPER_BOUND;
            beta = std::min(beta, score);
            if (alpha >= beta) return score;
        }
    }

    bool is_maximizing_player = (current_player % 2 == 0);
    int best_score = is_maximizing_player ? -999 : 999;

    hand_t playable_cards = get_playable_cards(trick_led_suite, hands[current_player]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int new_points_so_far = trick_points_so_far + POINTS_TABLE[std::bit_width(card)];
        card_t new_winning_card = trick_winning_card;
        int new_winner_player = trick_winner_player;
        if ((card & (trick_led_suite | S)) > trick_winning_card) {
            new_winning_card = card;
            new_winner_player = current_player;
        }

        hands[current_player] ^= card;
        int current_move_value = solve3(
            hands,
            (current_player + 1) % 4,
            remaining_cards ^ card,
            alpha,
            beta,
            transposition_table,
            trick_led_suite,
            new_points_so_far,
            new_winning_card,
            new_winner_player
        );
        hands[current_player] ^= card;

        if (is_maximizing_player) {
            best_score = std::max(best_score, current_move_value);
            alpha = std::max(alpha, best_score);
        } else {
            best_score = std::min(best_score, current_move_value);
            beta = std::min(beta, best_score);
        }

        if (beta <= alpha) {
            break;
        }
    }

    int flag;
    if (best_score <= initial_alpha) {
        flag = FLAG_UPPER_BOUND;
    } else if (best_score >= beta) {
        flag = FLAG_LOWER_BOUND;
    } else {
        flag = FLAG_EXACT;
    }
    transposition_table[state_key] = best_score | flag;

    return best_score;
}

int solve3(
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
) {
    int initial_alpha = alpha;
    uint64_t state_key = transposition_key(remaining_cards, current_player, trick_led_suite, trick_points_so_far, trick_winning_card);
    auto it = transposition_table.find(state_key);
    if (it != transposition_table.end()) {
        int entry = it->second;
        if (entry & FLAG_EXACT) {
            return entry ^ FLAG_EXACT;
        } else if (entry & FLAG_LOWER_BOUND) {
            int score = entry ^ FLAG_LOWER_BOUND;
            alpha = std::max(alpha, score);
            if (alpha >= beta) return score;
        } else if (entry & FLAG_UPPER_BOUND) {
            int score = entry ^ FLAG_UPPER_BOUND;
            beta = std::min(beta, score);
            if (alpha >= beta) return score;
        }
    }

    bool is_maximizing_player = (current_player % 2 == 0);
    int best_score = is_maximizing_player ? -999 : 999;

    hand_t playable_cards = get_playable_cards(trick_led_suite, hands[current_player]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int winner_player = trick_winner_player;
        if ((card & (trick_led_suite | S)) > trick_winning_card) {
            winner_player = current_player;
        }

        int trick_points = trick_points_so_far + POINTS_TABLE[std::bit_width(card)];
        if (remaining_cards == card) {
            trick_points += LAST_TRICK_BONUS;
        }
        if (winner_player % 2 != 0) {
            trick_points = 0;
        }
        int new_alpha = alpha - trick_points;
        int new_beta = beta - trick_points;

        hands[current_player] ^= card;
        int sub_game_value = solve0(
            hands,
            winner_player,
            remaining_cards ^ card,
            new_alpha,
            new_beta,
            transposition_table
        );
        hands[current_player] ^= card;

        int current_move_value = trick_points + sub_game_value;
        if (is_maximizing_player) {
            best_score = std::max(best_score, current_move_value);
            alpha = std::max(alpha, best_score);
        } else {
            best_score = std::min(best_score, current_move_value);
            beta = std::min(beta, best_score);
        }

        if (beta <= alpha) {
            break;
        }
    }

    int flag;
    if (best_score <= initial_alpha) {
        flag = FLAG_UPPER_BOUND;
    } else if (best_score >= beta) {
        flag = FLAG_LOWER_BOUND;
    } else {
        flag = FLAG_EXACT;
    }
    transposition_table[state_key] = best_score | flag;

    return best_score;
}

uint64_t transposition_key(hand_t remaining_cards, int current_player, suit_t trick_led_suite, int trick_points_so_far, card_t trick_winning_card) {
    uint64_t key = 0;
    key |= (remaining_cards << 28); // 36 bits for remaining_cards (up to 2^36-1)
    key |= (static_cast<uint64_t>(current_player) << 26); // 2 bits for current_player (0-3)
    key |= (static_cast<uint64_t>(std::bit_width(trick_led_suite)) << 20); // 6 bits for trick_led_suite (0-36)
    key |= (static_cast<uint64_t>(trick_points_so_far) << 14); // 6 bits for trick_points_so_far (0-63)
    key |= (static_cast<uint64_t>(std::bit_width(trick_winning_card)) << 8); // 6 bits for trick_winning_card (0-36)
    return key;
}

int get_stock_bonus(hand_t hand) {
    const card_t KING_OF_TRUMP = (1ULL << 32); // King of Spades
    const card_t QUEEN_OF_TRUMP = (1ULL << 31); // Queen of Spades
    if ((hand & KING_OF_TRUMP) && (hand & QUEEN_OF_TRUMP)) {
        return 20;
    }
    return 0;
}

int solve_deal(std::array<hand_t, 4>& hands) {
    for (size_t i = 0; i < hands.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (hands[i] & hands[j]) {
                throw std::runtime_error("Overlapping hands");
            }
        }
    }
    boost::unordered_flat_map<uint64_t, int> transposition_table;
    hand_t remaining_cards = std::accumulate(hands.begin(), hands.end(), (hand_t)0);

    int final_score = solve0(
        hands,
        0,
        remaining_cards,
        -999,
        999,
        transposition_table
    );
    final_score += get_stock_bonus(hands[0]); // Add bonus for player 0
    final_score += get_stock_bonus(hands[2]); // Add bonus for player 2
    return final_score;
}

} // namespace jass
