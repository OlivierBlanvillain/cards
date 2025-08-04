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

void test_CardRepresentation() {
    REQUIRE(simulation::c("JS") > simulation::c("9S"));
    REQUIRE(simulation::c("9S") > simulation::c("AS"));
    REQUIRE(simulation::c("AS") > simulation::c("KS"));
    REQUIRE(simulation::c("KS") > simulation::c("QS"));
    REQUIRE(simulation::c("QS") > simulation::c("10S"));
    REQUIRE(simulation::c("10S") > simulation::c("8S"));
    REQUIRE(simulation::c("8S") > simulation::c("7S"));
    REQUIRE(simulation::c("7S") > simulation::c("6S"));
    REQUIRE(simulation::c("AH") > simulation::c("KH"));
    REQUIRE(simulation::c("6C") == (1ULL << 0));
    REQUIRE(simulation::c("JS") == (1ULL << 35));
}

void test_GetPoints() {
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("AC"))] == 11);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("10C"))] == 10);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("KC"))] == 4);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("QC"))] == 3);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("JC"))] == 2);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("9C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("8C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("7C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("JS"))] == 20);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::c("9S"))] == 14);
}

void test_GetPlayableCards() {
    REQUIRE(jass::get_playable_cards(jass::D, simulation::c("KD,AC,JS")) == simulation::c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, simulation::c("KD,AS,JS")) == simulation::c("AS,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, simulation::c("KD,JS")) == simulation::c("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("JS,7S,9C")) == simulation::c("JS,7S,9C"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("JS,8S,AC")) == simulation::c("JS,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, simulation::c("JS,QS")) == simulation::c("JS,QS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("9S,8S,AC")) == simulation::c("9S,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("QH,JS,AC")) == simulation::c("QH,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("9S,AC,KC")) == simulation::c("9S,AC,KC"));
    REQUIRE(jass::get_playable_cards(jass::D, simulation::c("JH,9H,8C")) == simulation::c("JH,9H,8C"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::c("JS,7S,AC")) == simulation::c("JS,7S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, simulation::c("9S,8S,QH")) == simulation::c("9S,8S,QH"));
}

void test_SolveDeal() {
    std::array<jass::hand_t, 4> hands;

    hands = {
        simulation::c("10H,JS,7C,AD"),
        simulation::c("JH,KS,QC,8D"),
        simulation::c("AH,9S,8C,KD"),
        simulation::c("QS,8S,AC,10D")
    };
    auto result = jass::solve_deal(hands);
    REQUIRE(result == 94);

    hands = {
        simulation::c("10H,JS,7C,AD"),
        simulation::c("JH,QC,KS,8D"),
        simulation::c("9S,AH,7H,10C"),
        simulation::c("QS,AC,8S,10D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 100);

    hands = {
        simulation::c("8C,JS,QS,AS,JD,8H,AH,10H"),
        simulation::c("JC,8S,KS,9S,QD,QH,7H,JH"),
        simulation::c("KC,10S,7C,9H,9D,KD,AC,10C"),
        simulation::c("9C,7S,AD,KH,10D,7D,QC,8D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 112);

    hands = {
        simulation::c("9S,QS,8S,6S,AH,8H,8D,7D"),
        simulation::c("AS,10S,KH,KC,10C,8C,KD,JD"),
        simulation::c("6H,AC,QC,JC,6C,QD,10D,9D"),
        simulation::c("QH,JH,9H,7H,9C,7C,AD,6D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 83);
}

void test_GetStockBonus() {
    jass::hand_t hand_with_stock = simulation::c("KS,QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_with_stock) == 20);

    jass::hand_t hand_without_stock_king = simulation::c("KS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_king) == 0);

    jass::hand_t hand_without_stock_queen = simulation::c("QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_queen) == 0);

    jass::hand_t hand_without_stock_other_suit = simulation::c("KH,QH,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_other_suit) == 0);
}

void test_SolveDealWithFullStockGame() {
    std::array<jass::hand_t, 4> hands;
    hands = {simulation::c("KS,QS,AS,10S,9S,JS,8S,7S,6S"), simulation::c("AC,KC,QC,JC,10C,9C,8C,7C,6C"), simulation::c("AH,KH,QH,JH,10H,9H,8H,7H,6H"), simulation::c("AD,KD,QD,JD,10D,9D,8D,7D,6D")};
    REQUIRE(jass::solve_deal(hands) == 177);
}

void test_SwapTrumpOne() {
    auto befor = simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    auto after = simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::S) == after);

    befor = simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::c("10D,9H,9S,KS,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::D) == after);

    befor = simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::c("10H,9S,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::H) == after);

    befor = simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::c("10C,9H,9D,KD,AS,10S,KS,7S,6S");
    REQUIRE(simulation::swap_trump_one(befor, jass::C) == after);
}

void test_BestTrump() {
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("10S,9H,9D,KD,AC,10C,KC,7C,6C")) == jass::C);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("6C,7D,8D,6H,7H,8H,JH,6S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("8C,10C,6D,9H,JH,AH,6S,8S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("6C,QC,10D,KD,AD,QH,6S,QS,KS")) == jass::S); //?!
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("QC,6D,7D,QD,10H,7S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("6C,7C,10H,6S,7S,8S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("9C,AC,7D,QD,KD,AD,AH,QS,AS")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("KC,8D,6H,9H,6S,7S,8S,QS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("AC,8D,9D,10H,KH,7S,QS,AS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("6C,KC,JD,8S,6H,7H,8H,QH,JH")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("7D,KD,8H,KH,AH,8S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("KC,6H,KH,6D,7D,8D,QD,KD,JD")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("KC,7D,10D,AD,6S,10S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::c("8C,KC,KD,AD,AH,6S,QS,AS,JS")) == jass::S);
}

void test_CardRoundTrip() {
    std::string card_str = "AS";
    REQUIRE(simulation::d(simulation::c(card_str)) == card_str);

    card_str = "10H";
    REQUIRE(simulation::d(simulation::c(card_str)) == card_str);

    card_str = "7C";
    REQUIRE(simulation::d(simulation::c(card_str)) == card_str);

    card_str = "JD";
    REQUIRE(simulation::d(simulation::c(card_str)) == card_str);

    card_str = "JS,9S,AS,KS,QS,10S,8S,7S,6S,AH,KH,QH,JH,10H,9H,8H,7H,6H,AD,KD,QD,JD,10D,9D,8D,7D,6D,AC,KC,QC,JC,10C,9C,8C,7C,6C";
    REQUIRE(simulation::hand_to_string(simulation::c(card_str)) == card_str);
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
    RUN_TEST(test_CardRoundTrip);
    std::cout << "All tests passed!" << std::endl;
    return 0;
}
