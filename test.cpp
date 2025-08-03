#include "jass.h"
#include "simulation.h"

#include <cassert>
#include <iostream>
#include <map>
#include <bitset>

#define RUN_TEST(test_func) \
    std::cout << "Running test: " << #test_func << std::endl; \
    test_func();

#define REQUIRE(condition) \
    if (!(condition)) { \
        std::cerr << "Assertion failed in " << __func__ << ": " << #condition << " at " << __FILE__ << ":" << __LINE__ << std::endl; \
        exit(1); \
    }

std::map<std::pair<jass::suit_t, std::string>, int> CARD_TO_BIT = [](){
    std::map<std::pair<jass::suit_t, std::string>, int> the_map;
    const char* RANKS_TRUMP[] = {"J", "9", "A", "K", "Q", "10", "8", "7", "6"};
    const char* RANKS_PLAIN[] = {"A", "K", "Q", "J", "10", "9", "8", "7", "6"};

    int bit = 35; // 0-indexed bit position
    for (auto const& [suit_val, suit_char_val] : std::vector<std::pair<jass::suit_t, char>>{{jass::S, 'S'}, {jass::H, 'H'}, {jass::D, 'D'}, {jass::C, 'C'}}) {
        const char** ranks = (suit_val == jass::S) ? RANKS_TRUMP : RANKS_PLAIN;
        int num_ranks = 9;
        for (int i = 0; i < num_ranks; ++i) {
            the_map[{suit_val, ranks[i]}] = bit;
            bit--;
        }
    }
    return the_map;
}();

std::map<int, std::tuple<jass::suit_t, std::string, char>> BIT_TO_CARD = [](){
    std::map<int, std::tuple<jass::suit_t, std::string, char>> the_map;
    const char* RANKS_TRUMP[] = {"J", "9", "A", "K", "Q", "10", "8", "7", "6"};
    const char* RANKS_PLAIN[] = {"A", "K", "Q", "J", "10", "9", "8", "7", "6"};

    int bit = 35; // 0-indexed bit position
    for (auto const& [suit_val, suit_char_val] : std::vector<std::pair<jass::suit_t, char>>{{jass::S, 'S'}, {jass::H, 'H'}, {jass::D, 'D'}, {jass::C, 'C'}}) {
        const char** ranks = (suit_val == jass::S) ? RANKS_TRUMP : RANKS_PLAIN;
        int num_ranks = 9;
        for (int i = 0; i < num_ranks; ++i) {
            the_map[bit] = {suit_val, ranks[i], suit_char_val};
            bit--;
        }
    }
    return the_map;
}();

jass::card_t c(const std::string& desc) {
    jass::card_t total = 0;
    if (desc.empty()) return total;
    size_t start = 0;
    size_t end = desc.find(',');
    while (end != std::string::npos) {
        std::string token = desc.substr(start, end - start);
        std::string rank = token.substr(0, token.length() - 1);
        char suit_char = token.back();
        jass::suit_t suit;
        if (suit_char == 'C') suit = jass::C;
        else if (suit_char == 'D') suit = jass::D;
        else if (suit_char == 'H') suit = jass::H;
        else suit = jass::S;
        total |= 1ULL << CARD_TO_BIT.at({suit, rank});
        start = end + 1;
        end = desc.find(',', start);
    }
    std::string token = desc.substr(start);
    std::string rank = token.substr(0, token.length() - 1);
    char suit_char = token.back();
    jass::suit_t suit;
    if (suit_char == 'C') {
        suit = jass::C;
    } else if (suit_char == 'D') {
        suit = jass::D;
    } else if (suit_char == 'H') {
        suit = jass::H;
    } else {
        suit = jass::S;
    }
    total |= 1ULL << CARD_TO_BIT.at({suit, rank});
    return total;
}

std::string d(jass::card_t card_mask) {
    if (card_mask == 0) return "";
    int bit_pos = std::bit_width(card_mask); // This is 1-indexed bit_length (0-36)
    if (bit_pos == 0) return ""; // Handle NOT_A_CARD case
    auto const& [suit, rank, suit_char] = BIT_TO_CARD.at(bit_pos - 1); // Convert to 0-indexed for BIT_TO_CARD
    return rank + suit_char;
}

std::string hand_to_string(jass::hand_t hand) {
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

void test_CardRepresentation() {
    REQUIRE(c("JS") > c("9S"));
    REQUIRE(c("9S") > c("AS"));
    REQUIRE(c("AS") > c("KS"));
    REQUIRE(c("KS") > c("QS"));
    REQUIRE(c("QS") > c("10S"));
    REQUIRE(c("10S") > c("8S"));
    REQUIRE(c("8S") > c("7S"));
    REQUIRE(c("7S") > c("6S"));
    REQUIRE(c("AH") > c("KH"));
    REQUIRE(c("6C") == (1ULL << 0));
    REQUIRE(c("JS") == (1ULL << 35));
}

void test_GetPoints() {
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("AC"))] == 11);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("10C"))] == 10);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("KC"))] == 4);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("QC"))] == 3);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("JC"))] == 2);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("9C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("8C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("7C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("JS"))] == 20);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(c("9S"))] == 14);
}

void test_GetPlayableCards() {
    REQUIRE(jass::get_playable_cards(jass::D, c("KD,AC,JS")) == c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, c("KD,AS,JS")) == c("AS,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, c("KD,JS")) == c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, c("JS,7S,9C")) == c("JS,7S,9C"));
    REQUIRE(jass::get_playable_cards(jass::H, c("JS,8S,AC")) == c("JS,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, c("JS,QS")) == c("JS,QS"));
    REQUIRE(jass::get_playable_cards(jass::H, c("9S,8S,AC")) == c("9S,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::H, c("QH,JS,AC")) == c("QH,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, c("9S,AC,KC")) == c("9S,AC,KC"));
    REQUIRE(jass::get_playable_cards(jass::D, c("JH,9H,8C")) == c("JH,9H,8C"));
    REQUIRE(jass::get_playable_cards(jass::H, c("JS,7S,AC")) == c("JS,7S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, c("9S,8S,QH")) == c("9S,8S,QH"));
}

void test_SolveDeal() {
    std::array<jass::hand_t, 4> hands;

    hands = {
        c("10H,JS,7C,AD"),
        c("JH,KS,QC,8D"),
        c("AH,9S,8C,KD"),
        c("QS,8S,AC,10D")
    };
    auto result = jass::solve_deal(hands);
    REQUIRE(result.first == 94);
    REQUIRE(result.second == 630);

    hands = {
        c("10H,JS,7C,AD"),
        c("JH,QC,KS,8D"),
        c("9S,AH,7H,10C"),
        c("QS,AC,8S,10D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result.first == 100);
    REQUIRE(result.second == 1426);

    hands = {
        c("8C,JS,QS,AS,JD,8H,AH,10H"),
        c("JC,8S,KS,9S,QD,QH,7H,JH"),
        c("KC,10S,7C,9H,9D,KD,AC,10C"),
        c("9C,7S,AD,KH,10D,7D,QC,8D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result.first == 112);
    REQUIRE(result.second == 25385302);

    hands = {
        c("9S,QS,8S,6S,AH,8H,8D,7D"),
        c("AS,10S,KH,KC,10C,8C,KD,JD"),
        c("6H,AC,QC,JC,6C,QD,10D,9D"),
        c("QH,JH,9H,7H,9C,7C,AD,6D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result.first == 83);
    REQUIRE(result.second == 52505570);
}

void test_GetStockBonus() {
    jass::hand_t hand_with_stock = c("KS,QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_with_stock) == 20);

    jass::hand_t hand_without_stock_king = c("KS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_king) == 0);

    jass::hand_t hand_without_stock_queen = c("QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_queen) == 0);

    jass::hand_t hand_without_stock_other_suit = c("KH,QH,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_other_suit) == 0);
}

void test_SolveDealWithFullStockGame() {
    std::array<jass::hand_t, 4> hands;
    hands = {c("KS,QS,AS,10S,9S,JS,8S,7S,6S"), c("AC,KC,QC,JC,10C,9C,8C,7C,6C"), c("AH,KH,QH,JH,10H,9H,8H,7H,6H"), c("AD,KD,QD,JD,10D,9D,8D,7D,6D")};
    REQUIRE(jass::solve_deal(hands).first == 177);
}

void test_SwapTrumpOne() {
    auto befor = c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    auto after = c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::S) == after);

    befor = c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = c("10D,9H,9S,KS,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::D) == after);

    befor = c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = c("10H,9S,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::H) == after);

    befor = c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = c("10C,9H,9D,KD,AS,10S,KS,7S,6S");
    REQUIRE(simulation::swap_trump_one(befor, jass::C) == after);
}

void test_BestTrump() {
    REQUIRE(simulation::best_trump_quick_eval(c("10S,9H,9D,KD,AC,10C,KC,7C,6C")) == jass::C);
    REQUIRE(simulation::best_trump_quick_eval(c("6C,7D,8D,6H,7H,8H,JH,6S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(c("8C,10C,6D,9H,JH,AH,6S,8S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(c("6C,QC,10D,KD,AD,QH,6S,QS,KS")) == jass::S); //?!
    REQUIRE(simulation::best_trump_quick_eval(c("QC,6D,7D,QD,10H,7S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("6C,7C,10H,6S,7S,8S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("9C,AC,7D,QD,KD,AD,AH,QS,AS")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(c("KC,8D,6H,9H,6S,7S,8S,QS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("AC,8D,9D,10H,KH,7S,QS,AS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("6C,KC,JD,8S,6H,7H,8H,QH,JH")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(c("7D,KD,8H,KH,AH,8S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("KC,6H,KH,6D,7D,8D,QD,KD,JD")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(c("KC,7D,10D,AD,6S,10S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(c("8C,KC,KD,AD,AH,6S,QS,AS,JS")) == jass::S);
}

int main() {
    RUN_TEST(test_CardRepresentation);
    RUN_TEST(test_GetPoints);
    RUN_TEST(test_GetPlayableCards);
    RUN_TEST(test_SolveDeal);
    RUN_TEST(test_GetStockBonus);
    RUN_TEST(test_SolveDealWithFullStockGame);
    RUN_TEST(test_SwapTrumpOne);
    RUN_TEST(test_BestTrump);
    std::cout << "All tests passed!" << std::endl;
    return 0;
}
