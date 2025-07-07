import functools

C = 0x000000FF  # Clubs
D = 0x0000FF00  # Diamonds
H = 0x00FF0000  # Hearts
S = 0xFF000000  # Spades (trump suit)

POINTS_TABLE = [
    -1, # unused
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 3, 4, 10, 11, 14, 20,
]

SUITE_TABLE = [
    -1, # unused
    C, C, C, C, C, C, C, C,
    D, D, D, D, D, D, D, D,
    H, H, H, H, H, H, H, H,
    S, S, S, S, S, S, S, S,
]

def get_points(card: int) -> int:
    return POINTS_TABLE[card.bit_length()]

def get_suite(card: int) -> int:
    return SUITE_TABLE[card.bit_length()]

@functools.lru_cache(maxsize=None)
def get_trick_points(trick: tuple[int]) -> int:
    return sum(get_points(card) for card in trick)

def iter_bits(mask: int):
    while mask:
        b = mask & -mask
        yield b
        mask ^= b

@functools.lru_cache(maxsize=None)
def trick_winner(trick: tuple[int]) -> int:
    led_suit = get_suite(trick[0])
    trick_mask = sum(trick)
    potential_winners = trick_mask & (S | led_suit)
    winning_card_mask = 1 << (potential_winners.bit_length() - 1)
    return trick.index(winning_card_mask)

def get_playable_cards(trick: tuple[int], hand: int) -> int:
    if not trick:
        return hand

    # if there is a trump in the trick, players cannot purposefully undertrump
    trumps_in_trick = sum(trick) & S
    if trumps_in_trick:
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
            hand = (hand & ~S) | overtrumps  # remove undertrumps

    # players must follow suite
    cards_in_led_suit = hand & get_suite(trick[0])
    if cards_in_led_suit:
        return cards_in_led_suit

    # otherwise, players must play trump (unless their partner is winning)
    trumps_in_hand = hand & S
    if trumps_in_hand and (trick_winner(trick) != (len(trick) - 2)):
        return trumps_in_hand

    return hand

@functools.lru_cache(maxsize=None)
def double_dummy_solver(
  trick: tuple[int],
  hands: tuple[int],
  curr_player: int,
  use_alpha_beta: bool,
  alpha: int = -999,
  beta: int = 999,
) -> int:
    if sum(hands) == 0:
        return 0

    leading_player = (4 + curr_player - len(trick)) % 4


    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = get_playable_cards(trick, hands[curr_player])
    assert playable_cards != 0

    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        new_trick = trick + (card,)

        current_move_value: int
        if len(new_trick) == 4:
            winner_idx_in_trick = trick_winner(new_trick)
            winner_player = (leading_player + winner_idx_in_trick) % 4
            points = get_trick_points(new_trick)

            points_this_trick = 0
            if winner_player % 2 == 0:
                points_this_trick = points

            new_alpha, new_beta = alpha, beta
            new_alpha = alpha - points_this_trick
            new_beta = beta - points_this_trick

            sub_game_value = double_dummy_solver(
                trick=(),
                hands=tuple(new_hands),
                curr_player=winner_player,
                use_alpha_beta=use_alpha_beta,
                alpha=new_alpha,
                beta=new_beta,
            )
            current_move_value = points_this_trick + sub_game_value
        else:
            next_player = (curr_player + 1) % 4
            current_move_value = double_dummy_solver(
                trick=new_trick,
                hands=tuple(new_hands),
                curr_player=next_player,
                use_alpha_beta=use_alpha_beta,
                alpha=alpha,
                beta=beta,
            )

        if is_maximizing_player:
            best_score = max(best_score, current_move_value)
            if use_alpha_beta:
                alpha = max(alpha, best_score)
                if beta <= alpha:
                    break
        else:
            best_score = min(best_score, current_move_value)
            if use_alpha_beta:
                beta = min(beta, best_score)
                if beta <= alpha:
                    break

    return best_score
