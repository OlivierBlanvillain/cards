from jass import S, C, D, H
from utils import iter_bits

import random

CARD_LIST = [1 << i for i in range(36)]
ALL_CARDS = sum(CARD_LIST)
SUIT_BIT_START = {C: 0, D: 9, H: 18, S: 27}

def _initialize_swap_maps():
    T_TO_S = {}
    S_TO_T = {}
    ti_to_si = {0: 0, 1: 1, 2: 2, 3: 7, 4: 3, 5: 8, 6: 4, 7: 5, 8: 6}
    for t_suit in [C, D, H]:
        T_TO_S[t_suit] = {}
        S_TO_T[t_suit] = {}
        t_start = SUIT_BIT_START[t_suit]
        s_start = SUIT_BIT_START[S]
        for ti, si in ti_to_si.items():
            t_card = 1 << (t_start + ti)
            s_card = 1 << (s_start + si)
            T_TO_S[t_suit][t_card] = s_card
            S_TO_T[t_suit][s_card] = t_card
    return T_TO_S, S_TO_T

T_TO_S, S_TO_T = _initialize_swap_maps()

def swap_trump(hands: list[int], trump: int) -> list[int]:
    if trump == S:
        return hands
    new_hands = []
    for hand in hands:
        spades_cards = hand & S
        trump_cards = hand & trump
        stable_cards = hand & ~(trump | S)
        for card in iter_bits(spades_cards):
            ti = S_TO_T[trump][card]
            stable_cards |= ti
        for card in iter_bits(trump_cards):
            si = T_TO_S[trump][card]
            stable_cards |= si
        new_hands.append(stable_cards)
    return new_hands

def shuffle_one_hand() -> int:
    chosen_cards = random.sample(CARD_LIST, k=9)
    return sum(chosen_cards)

def shuffle_other_hands(declarer_hand: int) -> list[int]:
    remaining_cards = ALL_CARDS ^ declarer_hand
    cards_to_deal = [
        card
        for i in range(36)
        if remaining_cards & (card := (1 << i))
    ]
    random.shuffle(cards_to_deal)
    return [
        sum(cards_to_deal[0:9]),
        sum(cards_to_deal[9:18]),
        sum(cards_to_deal[18:27]),
    ]
