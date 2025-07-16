#include "jass.h"
#include <cassert>
#include <iostream>
#include <vector>
#include <numeric>

// Helper for printing test results
#define RUN_TEST(test_func) \
    std::cout << "Running test: " << #test_func << std::endl; \
    test_func();

// Helper for assertions
#define REQUIRE(condition) \
    if (!(condition)) { \
        std::cerr << "Assertion failed in " << __func__ << ": " << #condition << " at " << __FILE__ << ":" << __LINE__ << std::endl; \
        exit(1); \
    }

void test_CardRepresentation() {
    REQUIRE(jass::c("JS") > jass::c("9S"));
    REQUIRE(jass::c("9S") > jass::c("AS"));
    REQUIRE(jass::c("AS") > jass::c("KS"));
    REQUIRE(jass::c("KS") > jass::c("QS"));
    REQUIRE(jass::c("QS") > jass::c("10S"));
    REQUIRE(jass::c("10S") > jass::c("8S"));
    REQUIRE(jass::c("8S") > jass::c("7S"));
    REQUIRE(jass::c("7S") > jass::c("6S"));
    REQUIRE(jass::c("AH") > jass::c("KH"));
    REQUIRE(jass::c("6C") == (1ULL << 0));
    REQUIRE(jass::c("JS") == (1ULL << 35));
    REQUIRE(jass::F == (jass::S ^ (1ULL << 35)));
}

void test_GetPoints() {
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("AC"))] == 11);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("10C"))] == 10);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("KC"))] == 4);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("QC"))] == 3);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("JC"))] == 2);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("9C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("8C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("7C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("JS"))] == 20);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(jass::c("9S"))] == 14);
}

void test_TrickWinner() {
    REQUIRE(jass::trick_winner(jass::c("AH"), jass::c("7S"), jass::c("QH"), jass::c("10H")) == 1);
    REQUIRE(jass::trick_winner(jass::c("AH"), jass::c("KH"), jass::c("7S"), jass::c("10H")) == 2);
    REQUIRE(jass::trick_winner(jass::c("AH"), jass::c("KH"), jass::c("QH"), jass::c("7S")) == 3);
    REQUIRE(jass::trick_winner(jass::c("AH"), jass::c("7S"), jass::c("8S"), jass::c("10H")) == 2);
    REQUIRE(jass::trick_winner(jass::c("AH"), jass::c("8S"), jass::c("7S"), jass::c("10H")) == 1);
    REQUIRE(jass::trick_winner(jass::c("7S"), jass::c("8S"), jass::c("9S"), jass::c("JS")) == 3);
    REQUIRE(jass::trick_winner(jass::c("JS"), jass::c("9S"), jass::c("8S"), jass::c("7S")) == 0);
    REQUIRE(jass::trick_winner(jass::c("7H"), jass::c("8H"), jass::c("9H"), jass::c("10H")) == 3);
    REQUIRE(jass::trick_winner(jass::c("7H"), jass::c("8H"), jass::c("10H"), jass::c("JH")) == 3);
}

void test_GetPlayableCards() {
    REQUIRE(jass::get_playable_cards(jass::D, jass::c("KD,AC,JS")) == jass::c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, jass::c("KD,AS,JS")) == jass::c("AS,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, jass::c("KD,JS")) == jass::c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("JS,7S,9C")) == jass::c("JS,7S,9C"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("JS,8S,AC")) == jass::c("JS,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, jass::c("JS,QS")) == jass::c("JS,QS"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("9S,8S,AC")) == jass::c("9S,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("QH,JS,AC")) == jass::c("QH,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("9S,AC,KC")) == jass::c("9S,AC,KC"));
    REQUIRE(jass::get_playable_cards(jass::D, jass::c("JH,9H,8C")) == jass::c("JH,9H,8C"));
    REQUIRE(jass::get_playable_cards(jass::H, jass::c("JS,7S,AC")) == jass::c("JS,7S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, jass::c("9S,8S,QH")) == jass::c("9S,8S,QH"));
}

void test_SolveDeal() {
    std::array<jass::hand_t, 4> hands;

    hands = {jass::c("10H,JS,7C,AD"), jass::c("JH,KS,QC,8D"), jass::c("AH,9S,8C,KD"), jass::c("QS,8S,AC,10D")};
    REQUIRE(jass::solve_deal(hands) == 94);

    hands = {jass::c("10H,JS,7C,AD"), jass::c("JH,QC,KS,8D"), jass::c("9S,AH,7H,10C"), jass::c("QS,AC,8S,10D")};
    REQUIRE(jass::solve_deal(hands) == 100);

    hands = {jass::c("8C,JS,QS,AS,JD,8H,AH,10H"), jass::c("JC,8S,KS,9S,QD,QH,7H,JH"), jass::c("KC,10S,7C,9H,9D,KD,AC,10C"), jass::c("9C,7S,AD,KH,10D,7D,QC,8D")};
    REQUIRE(jass::solve_deal(hands) == 112);

    hands = {jass::c("9S,QS,8S,6S,AH,8H,8D,7D"), jass::c("AS,10S,KH,KC,10C,8C,KD,JD"), jass::c("6H,AC,QC,JC,6C,QD,10D,9D"), jass::c("QH,JH,9H,7H,9C,7C,AD,6D")};
    REQUIRE(jass::solve_deal(hands) == 83);
}

void test_GetStockBonus() {
    jass::hand_t hand_with_stock = jass::c("KS,QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_with_stock) == 20);

    jass::hand_t hand_without_stock_king = jass::c("KS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_king) == 0);

    jass::hand_t hand_without_stock_queen = jass::c("QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_queen) == 0);

    jass::hand_t hand_without_stock_other_suit = jass::c("KH,QH,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_other_suit) == 0);
}

void test_SolveDealWithFullStockGame() {
    std::array<jass::hand_t, 4> hands;
    hands = {jass::c("KS,QS,AS,10S,9S,JS,8S,7S,6S"), jass::c("AC,KC,QC,JC,10C,9C,8C,7C,6C"), jass::c("AH,KH,QH,JH,10H,9H,8H,7H,6H"), jass::c("AD,KD,QD,JD,10D,9D,8D,7D,6D")};
    REQUIRE(jass::solve_deal(hands) == 177);
}

int main() {
    jass::initialize_card_maps();
    RUN_TEST(test_CardRepresentation);
    RUN_TEST(test_GetPoints);
    RUN_TEST(test_TrickWinner);
    RUN_TEST(test_GetPlayableCards);
    RUN_TEST(test_SolveDeal);
    RUN_TEST(test_GetStockBonus);
    RUN_TEST(test_SolveDealWithFullStockGame);
    std::cout << "All tests passed!" << std::endl;
    return 0;
}
