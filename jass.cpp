#include "jass.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <map> // For std::map
#include "ankerl/unordered_dense.h"
#include <bit> // For std::countl_zero

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
static const hand_t FOLLOW_TABLE[37] = {
    0, // unused (index 0)
    C, C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H, H,
    F, F, F, F, F, F, F, F, F,
};

std::map<std::pair<Suit, std::string>, int> CARD_TO_BIT;
std::map<int, std::tuple<Suit, std::string, char>> BIT_TO_CARD;

hand_t get_suit(card_t card) {
    return SUIT_TABLE[std::bit_width(card)];
}

void initialize_card_maps() {
    const char* RANKS_TRUMP[] = {"J", "9", "A", "K", "Q", "10", "8", "7", "6"};
    const char* RANKS_PLAIN[] = {"A", "K", "Q", "J", "10", "9", "8", "7", "6"};

    int bit = 35; // 0-indexed bit position
    for (auto const& [suit_val, suit_name] : std::vector<std::pair<hand_t, Suit>>{{S, SPADES}, {H, HEARTS}, {D, DIAMONDS}, {C, CLUBS}}) {
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
    hand_t led_mask = S | led_suit;
    card2 &= led_mask;
    card3 &= led_mask;
    card4 &= led_mask;
    if (card1 >= card2 && card1 >= card3 && card1 >= card4) return 0;
    if (card2 >= card3 && card2 >= card4) return 1;
    if (card3 >= card4) return 2;
    return 3;
}

hand_t get_playable_cards(card_t card1, hand_t hand) {
    if (card1 == NOT_A_CARD) return hand;
    hand_t follow = hand & FOLLOW_TABLE[std::bit_width(card1)];
    if (follow) return follow | (hand & JACK_OF_TRUMP);
    return hand;
}

int solve0_0(std::array<uint64_t, 4>& cards_in_hand, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = remaining_cards;
    auto it = tt0.find(state_key);
    if (it != tt0.end()) {
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

    if (remaining_cards == 0) {
        return 0;
    }

    bool is_maximizing_player = true; // Player 0 is maximizing
    int best_score = -999;

    hand_t playable_cards = cards_in_hand[0];
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[0] ^= card;
        int current_move_value = solve1_1(
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[0] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt0[state_key] = best_score | flag;

    return best_score;
}

int solve0_1(std::array<uint64_t, 4>& cards_in_hand, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = remaining_cards;
    auto it = tt0.find(state_key);
    if (it != tt0.end()) {
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

    if (remaining_cards == 0) {
        return 0;
    }

    bool is_maximizing_player = false; // Player 1 is minimizing
    int best_score = 999;

    hand_t playable_cards = cards_in_hand[1];
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[1] ^= card;
        int current_move_value = solve1_2(
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[1] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt0[state_key] = best_score | flag;

    return best_score;
}

int solve0_2(std::array<uint64_t, 4>& cards_in_hand, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = remaining_cards;
    auto it = tt0.find(state_key);
    if (it != tt0.end()) {
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

    if (remaining_cards == 0) {
        return 0;
    }
    bool is_maximizing_player = true; // Player 2 is maximizing
    int best_score = -999;

    hand_t playable_cards = cards_in_hand[2];
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[2] ^= card;
        int current_move_value = solve1_3(
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[2] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt0[state_key] = best_score | flag;

    return best_score;
}

int solve0_3(std::array<uint64_t, 4>& cards_in_hand, uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = remaining_cards;
    auto it = tt0.find(state_key);
    if (it != tt0.end()) {
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

    if (remaining_cards == 0) {
        return 0;
    }

    
    int best_score = 999;

    hand_t playable_cards = cards_in_hand[3];
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[3] ^= card;
        int current_move_value = solve1_0(
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[3] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt0[state_key] = best_score | flag;

    return best_score;
}

int solve1_0(
    uint64_t card1,
    std::array<uint64_t, 4>& cards_in_hand,
    uint64_t remaining_cards,
    int alpha,
    int beta,
    ankerl::unordered_dense::map<uint64_t, int>& tt0,
    ankerl::unordered_dense::map<uint64_t, int>& tt1,
    ankerl::unordered_dense::map<uint64_t, int>& tt2,
    ankerl::unordered_dense::map<uint64_t, int>& tt3
){
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
    );
    auto it = tt1.find(state_key);
    if (it != tt1.end()) {
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

    bool is_maximizing_player = true; // Player 0 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[0]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[0] ^= card;
        int current_move_value = solve2_1(
            card1,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[0] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt1[state_key] = best_score | flag;

    return best_score;
}

int solve1_1(
    uint64_t card1,
    std::array<uint64_t, 4>& cards_in_hand,
    uint64_t remaining_cards,
    int alpha,
    int beta,
    ankerl::unordered_dense::map<uint64_t, int>& tt0,
    ankerl::unordered_dense::map<uint64_t, int>& tt1,
    ankerl::unordered_dense::map<uint64_t, int>& tt2,
    ankerl::unordered_dense::map<uint64_t, int>& tt3
){
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
    );
    auto it = tt1.find(state_key);
    if (it != tt1.end()) {
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

    bool is_maximizing_player = false; // Player 1 is minimizing
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[1]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[1] ^= card;
        int current_move_value = solve2_2(
            card1,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[1] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt1[state_key] = best_score | flag;

    return best_score;
}

int solve1_2(
    uint64_t card1,
    std::array<uint64_t, 4>& cards_in_hand,
    uint64_t remaining_cards,
    int alpha,
    int beta,
    ankerl::unordered_dense::map<uint64_t, int>& tt0,
    ankerl::unordered_dense::map<uint64_t, int>& tt1,
    ankerl::unordered_dense::map<uint64_t, int>& tt2,
    ankerl::unordered_dense::map<uint64_t, int>& tt3
){
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
    );
    auto it = tt1.find(state_key);
    if (it != tt1.end()) {
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

    bool is_maximizing_player = true; // Player 2 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[2]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[2] ^= card;
        int current_move_value = solve2_3(
            card1,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[2] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt1[state_key] = best_score | flag;

    return best_score;
}

int solve1_3(
    uint64_t card1,
    std::array<uint64_t, 4>& cards_in_hand,
    uint64_t remaining_cards,
    int alpha,
    int beta,
    ankerl::unordered_dense::map<uint64_t, int>& tt0,
    ankerl::unordered_dense::map<uint64_t, int>& tt1,
    ankerl::unordered_dense::map<uint64_t, int>& tt2,
    ankerl::unordered_dense::map<uint64_t, int>& tt3
){
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
    );
    auto it = tt1.find(state_key);
    if (it != tt1.end()) {
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

    
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[3]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[3] ^= card;
        int current_move_value = solve2_0(
            card1,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[3] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt1[state_key] = best_score | flag;

    return best_score;
}

int solve2_0(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
    );
    auto it = tt2.find(state_key);
    if (it != tt2.end()) {
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

    bool is_maximizing_player = true; // Player 0 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[0]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[0] ^= card;
        int current_move_value = solve3_1(
            card1,
            card2,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[0] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt2[state_key] = best_score | flag;

    return best_score;
}

int solve2_1(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
    );
    auto it = tt2.find(state_key);
    if (it != tt2.end()) {
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

    bool is_maximizing_player = false; // Player 1 is minimizing
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[1]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[1] ^= card;
        int current_move_value = solve3_2(
            card1,
            card2,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[1] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt2[state_key] = best_score | flag;

    return best_score;
}

int solve2_2(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
    );
    auto it = tt2.find(state_key);
    if (it != tt2.end()) {
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

    bool is_maximizing_player = true; // Player 2 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[2]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[2] ^= card;
        int current_move_value = solve3_3(
            card1,
            card2,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[2] ^= card;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt2[state_key] = best_score | flag;

    return best_score;
}

int solve2_3(uint64_t card1, uint64_t card2, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
    );
    auto it = tt2.find(state_key);
    if (it != tt2.end()) {
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

    
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[3]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        cards_in_hand[3] ^= card;
        int current_move_value = solve3_0(
            card1,
            card2,
            card,
            cards_in_hand,
            remaining_cards ^ card,
            alpha,
            beta,
            tt0, tt1, tt2, tt3
        );
        cards_in_hand[3] ^= card;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt2[state_key] = best_score | flag;

    return best_score;
}

int solve3_0(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
        | (std::bit_width(card3) << 2)
    );
    auto it = tt3.find(state_key);
    if (it != tt3.end()) {
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

    bool is_maximizing_player = true; // Player 0 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[0]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int current_move_value;
        int winner_idx_in_trick = trick_winner(card1, card2, card3, card);
        int points = get_trick_points(card1, card2, card3, card);
        if (remaining_cards == card) {
            points += LAST_TRICK_BONUS;
        }
        int points_this_trick = 0;
        if (winner_idx_in_trick % 2 == 0) { // Player 0 or 2 wins
            points_this_trick = points;
        }
        int new_alpha = alpha - points_this_trick;
        int new_beta = beta - points_this_trick;

        cards_in_hand[0] ^= card;
        int sub_game_value;
        if (winner_idx_in_trick == 0) { // Player 0 wins
            sub_game_value = solve0_0(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 1) { // Player 1 wins
            sub_game_value = solve0_1(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 2) { // Player 2 wins
            sub_game_value = solve0_2(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else { // winner_idx_in_trick == 3, Player 3 wins
            sub_game_value = solve0_3(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        }
        cards_in_hand[0] ^= card;

        current_move_value = points_this_trick + sub_game_value;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt3[state_key] = best_score | flag;

    return best_score;
}

int solve3_1(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
        | (std::bit_width(card3) << 2)
    );
    auto it = tt3.find(state_key);
    if (it != tt3.end()) {
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

    bool is_maximizing_player = false; // Player 1 is minimizing
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[1]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int current_move_value;
        int winner_idx_in_trick = trick_winner(card1, card2, card3, card);
        int points = get_trick_points(card1, card2, card3, card);
        if (remaining_cards == card) {
            points += LAST_TRICK_BONUS;
        }
        int points_this_trick = 0;
        if (winner_idx_in_trick % 2 == 1) { // Player 1 or 3 wins
            points_this_trick = points;
        }
        int new_alpha = alpha - points_this_trick;
        int new_beta = beta - points_this_trick;

        cards_in_hand[1] ^= card;
        int sub_game_value;
        if (winner_idx_in_trick == 0) { // Player 0 wins
            sub_game_value = solve0_0(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 1) { // Player 1 wins
            sub_game_value = solve0_1(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 2) { // Player 2 wins
            sub_game_value = solve0_2(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else { // winner_idx_in_trick == 3, Player 3 wins
            sub_game_value = solve0_3(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        }
        cards_in_hand[1] ^= card;

        current_move_value = points_this_trick + sub_game_value;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt3[state_key] = best_score | flag;

    return best_score;
}

int solve3_2(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
        | (std::bit_width(card3) << 2)
    );
    auto it = tt3.find(state_key);
    if (it != tt3.end()) {
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

    bool is_maximizing_player = true; // Player 2 is maximizing
    int best_score = -999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[2]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int current_move_value;
        int winner_idx_in_trick = trick_winner(card1, card2, card3, card);
        int points = get_trick_points(card1, card2, card3, card);
        if (remaining_cards == card) {
            points += LAST_TRICK_BONUS;
        }
        int points_this_trick = 0;
        if (winner_idx_in_trick % 2 == 0) { // Player 0 or 2 wins
            points_this_trick = points;
        }
        int new_alpha = alpha - points_this_trick;
        int new_beta = beta - points_this_trick;

        cards_in_hand[2] ^= card;
        int sub_game_value;
        if (winner_idx_in_trick == 0) { // Player 0 wins
            sub_game_value = solve0_0(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 1) { // Player 1 wins
            sub_game_value = solve0_1(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 2) { // Player 2 wins
            sub_game_value = solve0_2(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else { // winner_idx_in_trick == 3, Player 3 wins
            sub_game_value = solve0_3(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        }
        cards_in_hand[2] ^= card;

        current_move_value = points_this_trick + sub_game_value;

        best_score = std::max(best_score, current_move_value);
        alpha = std::max(alpha, best_score);

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
    tt3[state_key] = best_score | flag;

    return best_score;
}

int solve3_3(uint64_t card1, uint64_t card2, uint64_t card3, std::array<uint64_t, 4>& cards_in_hand,
           uint64_t remaining_cards, int alpha, int beta,
           ankerl::unordered_dense::map<uint64_t, int>& tt0,
           ankerl::unordered_dense::map<uint64_t, int>& tt1,
           ankerl::unordered_dense::map<uint64_t, int>& tt2,
           ankerl::unordered_dense::map<uint64_t, int>& tt3) {
    int initial_alpha = alpha;
    uint64_t state_key = (
        (remaining_cards << 20)
        | (std::bit_width(card1) << 14)
        | (std::bit_width(card2) << 8)
        | (std::bit_width(card3) << 2)
    );
    auto it = tt3.find(state_key);
    if (it != tt3.end()) {
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

    
    int best_score = 999;

    hand_t playable_cards = get_playable_cards(card1, cards_in_hand[3]);
    while (playable_cards) {
        card_t card = playable_cards & -playable_cards;
        playable_cards ^= card;

        int current_move_value;
        int winner_idx_in_trick = trick_winner(card1, card2, card3, card);
        int points = get_trick_points(card1, card2, card3, card);
        if (remaining_cards == card) {
            points += LAST_TRICK_BONUS;
        }
        int points_this_trick = 0;
        if (winner_idx_in_trick % 2 == 1) { // Player 1 or 3 wins
            points_this_trick = points;
        }
        int new_alpha = alpha - points_this_trick;
        int new_beta = beta - points_this_trick;

        cards_in_hand[3] ^= card;
        int sub_game_value;
        if (winner_idx_in_trick == 0) { // Player 0 wins
            sub_game_value = solve0_0(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 1) { // Player 1 wins
            sub_game_value = solve0_1(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else if (winner_idx_in_trick == 2) { // Player 2 wins
            sub_game_value = solve0_2(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        } else { // winner_idx_in_trick == 3, Player 3 wins
            sub_game_value = solve0_3(cards_in_hand, remaining_cards ^ card, new_alpha, new_beta, tt0, tt1, tt2, tt3);
        }
        cards_in_hand[3] ^= card;

        current_move_value = points_this_trick + sub_game_value;

        best_score = std::min(best_score, current_move_value);
        beta = std::min(beta, best_score);

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
    tt3[state_key] = best_score | flag;

    return best_score;
}

int solve_deal(std::array<hand_t, 4>& hands) {
    for (size_t i = 0; i < hands.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (hands[i] & hands[j]) {
                throw std::runtime_error("Overlapping hands");
            }
        }
    }
    ankerl::unordered_dense::map<uint64_t, int> tt0;
    ankerl::unordered_dense::map<uint64_t, int> tt1;
    ankerl::unordered_dense::map<uint64_t, int> tt2;
    ankerl::unordered_dense::map<uint64_t, int> tt3;

    // The initial call is to solve0
    int final_score = solve0_0(hands, std::accumulate(hands.begin(), hands.end(), (hand_t)0), -999, 999, tt0, tt1, tt2, tt3);
    return final_score;
}

} // namespace jass
