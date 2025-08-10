#include "jass.h"
#include <numeric>
#include <algorithm>
#include <map> // For std::map
#include <bit> // For std::bit_width
#include <string>
#include <boost/unordered/unordered_flat_map.hpp>

namespace jass {

// 1-indexed by bit_length (0-36)
const int POINTS_TABLE[37] = {
    -1, // unused (index 0)
    // 6, 7, 8, 9, 10, J, Q, K, As
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Diamonds (bit_length 1-9)
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Clubs (bit_length 10-18)
    0, 0, 0, 0, 10, 2, 3, 4, 11, // Hearts (bit_length 19-27)
    // 6, 7, 8, 10, Q, K, As, 9,  J
    0, 0, 0, 10, 3, 4, 11, 14, 20, // Spades (bit_length 28-36)
};

// 1-indexed by bit_length (0-36)
static const hand_t SUIT_TABLE[37] = {
    0, // unused (index 0)
    D, D, D, D, D, D, D, D, D,
    C, C, C, C, C, C, C, C, C,
    H, H, H, H, H, H, H, H, H,
    S, S, S, S, S, S, S, S, S,
};

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

template <int TRICK_DEPTH, int CURRENT_PLAYER>
int solve_trick(
    std::array<card_t, 4>& hands,
    hand_t remaining_cards,
    int alpha,
    int beta,
    std::array<std::array<boost::unordered_flat_map<uint32_t, int>, 16>, 4>& transposition_tables,
    suit_t trick_led_suit,
    int trick_points_so_far,
    card_t trick_winning_card,
    int trick_winner_player
) {
    // Base case for the entire game (only checked at the start of a trick)
    if constexpr (TRICK_DEPTH == 0) {
        if (remaining_cards == 0) {
            return 0;
        }
    }

    // --- Transposition Table Lookup ---

    int initial_alpha;
    if constexpr (TRICK_DEPTH == 0) {
        initial_alpha = alpha;
        uint32_t table_index = (remaining_cards >> 32) & 0xF; // Extract first 4 bits
        uint32_t state_key = remaining_cards & 0xFFFFFFFF; // Use remaining 32 bits
        auto it = transposition_tables[CURRENT_PLAYER][table_index].find(state_key);
        if (it != transposition_tables[CURRENT_PLAYER][table_index].end()) {
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
    }

    // --- Alpha-Beta Initialization ---
    constexpr bool is_maximizing_player = (CURRENT_PLAYER % 2 == 0);
    int best_score = is_maximizing_player ? -999 : 999;

    // --- Card Iteration ---
    hand_t playable_cards;
    if constexpr (TRICK_DEPTH == 0) {
        playable_cards = hands[CURRENT_PLAYER];
    } else {
        playable_cards = get_playable_cards(trick_led_suit, hands[CURRENT_PLAYER]);
    }

    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        hands[CURRENT_PLAYER] ^= card;
        int current_move_value;

        // --- Recursive Call Logic ---
        if constexpr (TRICK_DEPTH == 0) {
            // Starting a new trick (was solve0)
            int new_points_so_far = POINTS_TABLE[std::bit_width(card)];
            card_t new_winning_card = card;
            int new_winner_player = CURRENT_PLAYER;
            suit_t new_trick_led_suit = SUIT_TABLE[std::bit_width(card)];
            current_move_value = solve_trick<TRICK_DEPTH + 1, (CURRENT_PLAYER + 1) % 4>(hands, remaining_cards ^ card, alpha, beta, transposition_tables,  new_trick_led_suit, new_points_so_far, new_winning_card, new_winner_player);
        } else if constexpr (TRICK_DEPTH < 3) {
            // Continuing a trick
            int new_points_so_far = trick_points_so_far + POINTS_TABLE[std::bit_width(card)];
            card_t new_winning_card = trick_winning_card;
            int new_winner_player = trick_winner_player;
            if ((card & (trick_led_suit | S)) > trick_winning_card) {
                new_winning_card = card;
                new_winner_player = CURRENT_PLAYER;
            }
            current_move_value = solve_trick<TRICK_DEPTH + 1, (CURRENT_PLAYER + 1) % 4>(hands, remaining_cards ^ card, alpha, beta, transposition_tables,  trick_led_suit, new_points_so_far, new_winning_card, new_winner_player);
        } else { // TRICK_DEPTH == 3
            // Finishing a trick
            int winner_player_final = trick_winner_player;
            if ((card & (trick_led_suit | S)) > trick_winning_card) {
                winner_player_final = CURRENT_PLAYER;
            }

            int trick_points = trick_points_so_far + POINTS_TABLE[std::bit_width(card)];
            if (remaining_cards == card) { // is last card of the game
                trick_points += LAST_TRICK_BONUS;
            }

            if (winner_player_final % 2 != 0) {
                trick_points = 0;
            }

            int new_alpha = alpha - trick_points;
            int new_beta = beta - trick_points;

            // Recurse to start the next trick
            int sub_game_value;
            switch (winner_player_final) {
                case 0:
                    sub_game_value = solve_trick<0, 0>(hands, remaining_cards ^ card, new_alpha, new_beta, transposition_tables, NOT_A_SUIT, 0, NOT_A_CARD, -1);
                    break;
                case 1:
                    sub_game_value = solve_trick<0, 1>(hands, remaining_cards ^ card, new_alpha, new_beta, transposition_tables, NOT_A_SUIT, 0, NOT_A_CARD, -1);
                    break;
                case 2:
                    sub_game_value = solve_trick<0, 2>(hands, remaining_cards ^ card, new_alpha, new_beta, transposition_tables, NOT_A_SUIT, 0, NOT_A_CARD, -1);
                    break;
                default: // 3
                    sub_game_value = solve_trick<0, 3>(hands, remaining_cards ^ card, new_alpha, new_beta, transposition_tables, NOT_A_SUIT, 0, NOT_A_CARD, -1);
                    break;
            }

            current_move_value = trick_points + sub_game_value;
        }

        hands[CURRENT_PLAYER] ^= card; // Backtrack

        // --- Alpha-Beta Update ---
        if constexpr (is_maximizing_player) {
            best_score = std::max(best_score, current_move_value);
            alpha = std::max(alpha, best_score);
        } else {
            best_score = std::min(best_score, current_move_value);
            beta = std::min(beta, best_score);
        }

        if (beta <= alpha) {
            break; // Prune
        }
    }

    // --- Transposition Table Storage ---
    if constexpr (TRICK_DEPTH == 0) {
        int flag;
        if (best_score <= initial_alpha) {
            flag = FLAG_UPPER_BOUND;
        } else if (best_score >= beta) {
            flag = FLAG_LOWER_BOUND;
        } else {
            flag = FLAG_EXACT;
        }
        uint32_t table_index = (remaining_cards >> 32) & 0xF;
        uint32_t state_key = remaining_cards & 0xFFFFFFFF;
        transposition_tables[CURRENT_PLAYER][table_index][state_key] = best_score | flag;
    }

    return best_score;
}

int get_stock_bonus(hand_t hand) {
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
    std::array<std::array<boost::unordered_flat_map<uint32_t, int>, 16>, 4> transposition_tables;
    hand_t remaining_cards = std::accumulate(hands.begin(), hands.end(), (hand_t)0);

    // Initial call to solve_trick starts with NOT_A_SUIT as the led suit.
    int final_score = solve_trick<0, 0>(
        hands,
        remaining_cards,
        -999,
        999,
        transposition_tables,
        NOT_A_SUIT,
        0,
        NOT_A_CARD,
        -1
    );
    final_score += get_stock_bonus(hands[0]); // Add bonus for player 0
    final_score += get_stock_bonus(hands[2]); // Add bonus for player 2
    return final_score;
}

} // namespace jass
