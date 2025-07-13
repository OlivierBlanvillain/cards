import functools
from dataclasses import dataclass

FLAG_EXACT = 1 << 10
FLAG_LOWER_BOUND = 1 << 11
FLAG_UPPER_BOUND = 1 << 12

C = 0b000000000000000000000000000111111111  # Clubs
D = 0b000000000000000000111111111000000000  # Diamonds
H = 0b000000000111111111000000000000000000  # Hearts
S = 0b111111111000000000000000000000000000  # Spades (trump suit)

POINTS_TABLE = [
    -1, # unused
    # 6, 7, 8, 9, 10, J, Q, K, As
    0, 0, 0, 0, 10, 2, 3, 4, 11,
    0, 0, 0, 0, 10, 2, 3, 4, 11,
    0, 0, 0, 0, 10, 2, 3, 4, 11,
    # 6, 7, 8, 10, Q, K, As, 9,  J
    0, 0, 0, 10, 3, 4, 11, 14, 20,
]

SUIT_TABLE = [
    -1, # unused
    C, C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H, H,
    S, S, S, S, S, S, S, S, S,
]

NOT_A_CARD = 0
LAST_TRICK_BONUS = 5

JACK_OF_TRUMP = (1 << 35)
F = S ^ JACK_OF_TRUMP

FOLLOW_TABLE = [
    -1, # unused
    C, C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H, H,
    F, F, F, F, F, F, F, F, F,
]

def get_suit(card: int) -> int:
    return SUIT_TABLE[card.bit_length()]

# @functools.lru_cache(maxsize=None)
def get_trick_points(card1: int, card2: int, card3: int, card4: int) -> int:
    return (
        POINTS_TABLE[card1.bit_length()]
        + POINTS_TABLE[card2.bit_length()]
        + POINTS_TABLE[card3.bit_length()]
        + POINTS_TABLE[card4.bit_length()]
    )


# @functools.lru_cache(maxsize=None)
def trick_winner(card1: int, card2: int, card3: int, card4: int) -> int:
    led_suit = get_suit(card1)
    led_mask = S | led_suit
    card2 &= led_mask
    card3 &= led_mask
    card4 &= led_mask
    if card1 >= card2 and card1 >= card3 and card1 >= card4:
        return 0
    if card2 >= card3 and card2 >= card4:
        return 1
    if card3 >= card4:
        return 2
    return 3

@functools.lru_cache(maxsize=None)
def get_playable_cards(card1: int, hand: int) -> int:
    if card1 == NOT_A_CARD:
        return hand
    follow = hand & FOLLOW_TABLE[card1.bit_length()]
    if follow:
        return follow | (hand & JACK_OF_TRUMP)
    return hand



def double_dummy_solver(
    card1: int,
    card2: int,
    card3: int,
    hands: list[int],
    curr_player: int,
    remaining_cards: int,
    alpha: int,
    beta: int,
    transposition_table: dict[int, int],
) -> int:
    if remaining_cards == 0:
        return 0
    initial_alpha = alpha
    state_key = (
        (remaining_cards << 20)
        | (card1.bit_length() << 14)
        | (card2.bit_length() << 8)
        | (card3.bit_length() << 2)
        | curr_player
    )
    if entry := transposition_table.get(state_key):
        if entry & FLAG_EXACT:
            return entry ^ FLAG_EXACT
        elif entry & FLAG_LOWER_BOUND:
            score = entry ^ FLAG_LOWER_BOUND
            alpha = max(alpha, score)
            if alpha >= beta:
                return score
        elif entry & FLAG_UPPER_BOUND:
            score = entry ^ FLAG_UPPER_BOUND
            beta = min(beta, score)
            if alpha >= beta:
                return score
    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = get_playable_cards(card1, hands[curr_player])
    while playable_cards:
        card = playable_cards & -playable_cards
        playable_cards ^= card
        hands[curr_player] ^= card
        if card1 == NOT_A_CARD:
            current_move_value = double_dummy_solver(
                card1=card,
                card2=NOT_A_CARD,
                card3=NOT_A_CARD,
                hands=hands,
                curr_player=(curr_player + 1) % 4,
                remaining_cards=remaining_cards ^ card,
                alpha=alpha,
                beta=beta,
                transposition_table=transposition_table,
            )
        elif card2 == NOT_A_CARD:
            current_move_value = double_dummy_solver(
                card1=card1,
                card2=card,
                card3=NOT_A_CARD,
                hands=hands,
                curr_player=(curr_player + 1) % 4,
                remaining_cards=remaining_cards ^ card,
                alpha=alpha,
                beta=beta,
                transposition_table=transposition_table,
            )
        elif card3 == NOT_A_CARD:
            current_move_value = double_dummy_solver(
                card1=card1,
                card2=card2,
                card3=card,
                hands=hands,
                curr_player=(curr_player + 1) % 4,
                remaining_cards=remaining_cards ^ card,
                alpha=alpha,
                beta=beta,
                transposition_table=transposition_table,
            )
        else:
            winner_idx_in_trick = trick_winner(card1, card2, card3, card)
            winner_player = (curr_player + winner_idx_in_trick + 1) % 4
            points = get_trick_points(card1, card2, card3, card)
            if remaining_cards == card:
                points += LAST_TRICK_BONUS
            points_this_trick = 0
            if winner_player % 2 == 0:
                points_this_trick = points
            new_alpha, new_beta = alpha, beta
            new_alpha = alpha - points_this_trick
            new_beta = beta - points_this_trick
            sub_game_value = double_dummy_solver(
                card1=NOT_A_CARD,
                card2=NOT_A_CARD,
                card3=NOT_A_CARD,
                hands=hands,
                curr_player=winner_player,
                remaining_cards=remaining_cards ^ card,
                alpha=new_alpha,
                beta=new_beta,
                transposition_table=transposition_table,
            )
            current_move_value = points_this_trick + sub_game_value
        hands[curr_player] ^= card
        if is_maximizing_player:
            best_score = max(best_score, current_move_value)
            alpha = max(alpha, best_score)
        else:
            best_score = min(best_score, current_move_value)
            beta = min(beta, best_score)
        if beta <= alpha:
            break
    if best_score <= initial_alpha:
        flag = FLAG_UPPER_BOUND
    elif best_score >= beta:
        flag = FLAG_LOWER_BOUND
    else:
        flag = FLAG_EXACT
    transposition_table[state_key] = best_score | flag
    return best_score

def solve_deal(hands: list[int]) -> int:
    for i in range(len(hands)):
      for j in range(i):
        assert not hands[i] & hands[j], f"hands {i} and {j} overlap"

    return double_dummy_solver(
        card1=NOT_A_CARD,
        card2=NOT_A_CARD,
        card3=NOT_A_CARD,
        hands=hands,
        curr_player=0,
        remaining_cards=sum(hands),
        alpha=-999,
        beta=999,
        transposition_table={},
    )

# debug utils

RANKS_TRUMP = ['J', '9', 'A', 'K', 'Q', '10', '8', '7', '6']
RANKS_PLAIN = ['A', 'K', 'Q', 'J', '10', '9', '8', '7', '6']
CARD_TO_BIT = {}
BIT_TO_CARD = {}

bit = 35
for suit, suit_name in [(S, "♠"), (H, "♥"), (D, "♦"), (C, "♣")]:
    ranks = RANKS_TRUMP if suit == S else RANKS_PLAIN
    for rank in ranks:
        CARD_TO_BIT[(suit, rank)] = bit
        BIT_TO_CARD[bit] = (suit, rank, suit_name)
        bit -= 1

def c(desc: str) -> int:
    total = 0
    if not desc:
        return total
    for token in desc.split(','):
        rank = token[:-1]
        suit_char = token[-1]
        suit = {"♣": C, "♦": D, "♥": H, "♠": S}[suit_char]
        total |= 1 << CARD_TO_BIT[(suit, rank)]
    return total

def d(card_mask: int) -> str:
    if card_mask == 0:
        return ""
    bit_pos = card_mask.bit_length() - 1
    suit, rank, suit_char = BIT_TO_CARD[bit_pos]
    return rank + suit_char
