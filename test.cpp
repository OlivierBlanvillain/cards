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
    REQUIRE(simulation::string_to_hand("JS") > simulation::string_to_hand("9S"));
    REQUIRE(simulation::string_to_hand("9S") > simulation::string_to_hand("AS"));
    REQUIRE(simulation::string_to_hand("AS") > simulation::string_to_hand("KS"));
    REQUIRE(simulation::string_to_hand("KS") > simulation::string_to_hand("QS"));
    REQUIRE(simulation::string_to_hand("QS") > simulation::string_to_hand("10S"));
    REQUIRE(simulation::string_to_hand("10S") > simulation::string_to_hand("8S"));
    REQUIRE(simulation::string_to_hand("8S") > simulation::string_to_hand("7S"));
    REQUIRE(simulation::string_to_hand("7S") > simulation::string_to_hand("6S"));
    REQUIRE(simulation::string_to_hand("AH") > simulation::string_to_hand("KH"));
    REQUIRE(simulation::string_to_hand("6D") == (1ULL << 0));
    REQUIRE(simulation::string_to_hand("JS") == (1ULL << 35));
}

void test_GetPoints() {
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("AC"))] == 11);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("10C"))] == 10);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("KC"))] == 4);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("QC"))] == 3);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("JC"))] == 2);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("9C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("8C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("7C"))] == 0);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("JS"))] == 20);
    REQUIRE(jass::POINTS_TABLE[std::bit_width(simulation::string_to_hand("9S"))] == 14);
}

void test_GetPlayableCards() {
    REQUIRE(jass::get_playable_cards(jass::D, simulation::string_to_hand("KD,AC,JS")) == simulation::string_to_hand("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, simulation::string_to_hand("KD,AS,JS")) == simulation::string_to_hand("AS,JS"));
    REQUIRE(jass::get_playable_cards(jass::S, simulation::string_to_hand("KD,JS")) == simulation::string_to_hand("KD,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("JS,7S,9C")) == simulation::string_to_hand("JS,7S,9C"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("JS,8S,AC")) == simulation::string_to_hand("JS,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, simulation::string_to_hand("JS,QS")) == simulation::string_to_hand("JS,QS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("9S,8S,AC")) == simulation::string_to_hand("9S,8S,AC"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("QH,JS,AC")) == simulation::string_to_hand("QH,JS"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("9S,AC,KC")) == simulation::string_to_hand("9S,AC,KC"));
    REQUIRE(jass::get_playable_cards(jass::D, simulation::string_to_hand("JH,9H,8C")) == simulation::string_to_hand("JH,9H,8C"));
    REQUIRE(jass::get_playable_cards(jass::H, simulation::string_to_hand("JS,7S,AC")) == simulation::string_to_hand("JS,7S,AC"));
    REQUIRE(jass::get_playable_cards(jass::C, simulation::string_to_hand("9S,8S,QH")) == simulation::string_to_hand("9S,8S,QH"));
}

void test_SolveDeal() {
    std::array<jass::hand_t, 4> hands;

    hands = {
        simulation::string_to_hand("10H,JS,7C,AD"),
        simulation::string_to_hand("JH,KS,QC,8D"),
        simulation::string_to_hand("AH,9S,8C,KD"),
        simulation::string_to_hand("QS,8S,AC,10D")
    };
    auto result = jass::solve_deal(hands);
    REQUIRE(result == 94);

    hands = {
        simulation::string_to_hand("10H,JS,7C,AD"),
        simulation::string_to_hand("JH,QC,KS,8D"),
        simulation::string_to_hand("9S,AH,7H,10C"),
        simulation::string_to_hand("QS,AC,8S,10D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 100);

    hands = {
        simulation::string_to_hand("8C,JS,QS,AS,JD,8H,AH,10H"),
        simulation::string_to_hand("JC,8S,KS,9S,QD,QH,7H,JH"),
        simulation::string_to_hand("KC,10S,7C,9H,9D,KD,AC,10C"),
        simulation::string_to_hand("9C,7S,AD,KH,10D,7D,QC,8D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 112);

    hands = {
        simulation::string_to_hand("9S,QS,8S,6S,AH,8H,8D,7D"),
        simulation::string_to_hand("AS,10S,KH,KC,10C,8C,KD,JD"),
        simulation::string_to_hand("6H,AC,QC,JC,6C,QD,10D,9D"),
        simulation::string_to_hand("QH,JH,9H,7H,9C,7C,AD,6D")
    };
    result = jass::solve_deal(hands);
    REQUIRE(result == 83);
}

void test_GetStockBonus() {
    jass::hand_t hand_with_stock = simulation::string_to_hand("KS,QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_with_stock) == 20);

    jass::hand_t hand_without_stock_king = simulation::string_to_hand("KS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_king) == 0);

    jass::hand_t hand_without_stock_queen = simulation::string_to_hand("QS,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_queen) == 0);

    jass::hand_t hand_without_stock_other_suit = simulation::string_to_hand("KH,QH,AC,KD");
    REQUIRE(jass::get_stock_bonus(hand_without_stock_other_suit) == 0);
}

void test_SolveDealWithFullStockGame() {
    std::array<jass::hand_t, 4> hands;
    hands = {simulation::string_to_hand("KS,QS,AS,10S,9S,JS,8S,7S,6S"), simulation::string_to_hand("AC,KC,QC,JC,10C,9C,8C,7C,6C"), simulation::string_to_hand("AH,KH,QH,JH,10H,9H,8H,7H,6H"), simulation::string_to_hand("AD,KD,QD,JD,10D,9D,8D,7D,6D")};
    REQUIRE(jass::solve_deal(hands) == 177);
}

void test_SwapTrumpOne() {
    auto befor = simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    auto after = simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::S) == after);

    befor = simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::string_to_hand("10D,9H,9S,KS,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::D) == after);

    befor = simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::string_to_hand("10H,9S,9D,KD,AC,10C,KC,7C,6C");
    REQUIRE(simulation::swap_trump_one(befor, jass::H) == after);

    befor = simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C");
    after = simulation::string_to_hand("10C,9H,9D,KD,AS,10S,KS,7S,6S");
    REQUIRE(simulation::swap_trump_one(befor, jass::C) == after);
}

void test_BestTrump() {
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("10S,9H,9D,KD,AC,10C,KC,7C,6C")) == jass::C);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("6C,7D,8D,6H,7H,8H,JH,6S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("8C,10C,6D,9H,JH,AH,6S,8S,KS")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("6C,QC,10D,KD,AD,QH,6S,QS,KS")) == jass::S); //?!
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("QC,6D,7D,QD,10H,7S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("6C,7C,10H,6S,7S,8S,10S,QS,KS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("9C,AC,7D,QD,KD,AD,AH,QS,AS")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("KC,8D,6H,9H,6S,7S,8S,QS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("AC,8D,9D,10H,KH,7S,QS,AS,9S")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("6C,KC,JD,8S,6H,7H,8H,QH,JH")) == jass::H);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("7D,KD,8H,KH,AH,8S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("KC,6H,KH,6D,7D,8D,QD,KD,JD")) == jass::D);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("KC,7D,10D,AD,6S,10S,QS,KS,JS")) == jass::S);
    REQUIRE(simulation::best_trump_quick_eval(simulation::string_to_hand("8C,KC,KD,AD,AH,6S,QS,AS,JS")) == jass::S);
}

void test_CardRoundTrip() {
    std::string card_str = "AS";
    REQUIRE(simulation::hand_to_string(simulation::string_to_hand(card_str)) == card_str);

    card_str = "10H";
    REQUIRE(simulation::hand_to_string(simulation::string_to_hand(card_str)) == card_str);

    card_str = "7C";
    REQUIRE(simulation::hand_to_string(simulation::string_to_hand(card_str)) == card_str);

    card_str = "JD";
    REQUIRE(simulation::hand_to_string(simulation::string_to_hand(card_str)) == card_str);

    card_str = "JS,9S,AS,KS,QS,10S,8S,7S,6S,AH,KH,QH,JH,10H,9H,8H,7H,6H,AC,KC,QC,JC,10C,9C,8C,7C,6C,AD,KD,QD,JD,10D,9D,8D,7D,6D";
    REQUIRE(simulation::hand_to_string(simulation::string_to_hand(card_str)) == card_str);
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
