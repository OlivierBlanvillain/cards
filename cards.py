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
def get_trick_points(card1: int, card2: int, card3: int, card4: int) -> int:
    return get_points(card1) + get_points(card2) + get_points(card3) + get_points(card4)

def iter_bits(mask: int):
    while mask:
        b = mask & -mask
        yield b
        mask ^= b

def trick_winner(card1: int, card2: int, card3: int, card4: int) -> int:
    led_suit = get_suite(card1)
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
def get_playable_cards1(card1: int, hand: int) -> int:
    # if there is a trump in the trick, players cannot purposefully undertrump
    trumps_in_trick = card1 & S
    if trumps_in_trick:
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
            hand = (hand & ~S) | overtrumps  # remove undertrumps

    # players must follow suite
    cards_in_led_suit = hand & get_suite(card1)
    if cards_in_led_suit:
        return cards_in_led_suit

    # otherwise trump
    trumps_in_hand = hand & S
    if trumps_in_hand:
        return trumps_in_hand

    return hand

@functools.lru_cache(maxsize=None)
def get_playable_cards2(card1: int, card2: int, hand: int) -> int:
    # if there is a trump in the trick, players cannot purposefully undertrump
    trumps_in_trick = (card1 + card2) & S
    if trumps_in_trick:
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
            hand = (hand & ~S) | overtrumps  # remove undertrumps

    # players must follow suite
    led_suite = get_suite(card1)
    cards_in_led_suit = hand & led_suite
    if cards_in_led_suit:
        return cards_in_led_suit

    # otherwise, players must play trump (unless their partner is winning)
    partner_is_winning = card1 > (card2 & (led_suite | S))
    trumps_in_hand = hand & S
    if trumps_in_hand and not partner_is_winning:
        return trumps_in_hand

    return hand

@functools.lru_cache(maxsize=None)
def get_playable_cards3(card1: int, card2: int, card3: int, hand: int) -> int:
    # if there is a trump in the trick, players cannot purposefully undertrump
    trumps_in_trick = (card1 + card2 + card3) & S
    if trumps_in_trick:
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
            hand = (hand & ~S) | overtrumps  # remove undertrumps

    # players must follow suite
    led_suite = get_suite(card1)
    cards_in_led_suit = hand & led_suite
    if cards_in_led_suit:
        return cards_in_led_suit

    # otherwise, players must play trump (unless their partner is winning)
    relevant_mask = (led_suite | S)
    card2 = card2 & relevant_mask
    card3 = card3 & relevant_mask
    partner_is_winning = card2 > card1 and card2 > card3
    trumps_in_hand = hand & S
    if trumps_in_hand and not partner_is_winning:
        return trumps_in_hand

    return hand

@functools.lru_cache(maxsize=None)
def double_dummy_solver0(
  hands: tuple[int],
  curr_player: int,
  use_alpha_beta: bool,
  alpha: int = -999,
  beta: int = 999,
) -> int:
    if sum(hands) == 0:
        return 0
    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = hands[curr_player]
    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        current_move_value = double_dummy_solver1(
            card1=card,
            hands=tuple(new_hands),
            curr_player=(curr_player + 1) % 4,
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

@functools.lru_cache(maxsize=None)
def double_dummy_solver1(
  card1: int,
  hands: tuple[int],
  curr_player: int,
  use_alpha_beta: bool,
  alpha: int = -999,
  beta: int = 999,
) -> int:
    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = get_playable_cards1(card1, hands[curr_player])
    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        current_move_value = double_dummy_solver2(
            card1=card1,
            card2=card,
            hands=tuple(new_hands),
            curr_player=(curr_player + 1) % 4,
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

@functools.lru_cache(maxsize=None)
def double_dummy_solver2(
  card1: int,
  card2: int,
  hands: tuple[int],
  curr_player: int,
  use_alpha_beta: bool,
  alpha: int = -999,
  beta: int = 999,
) -> int:
    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = get_playable_cards2(card1, card2, hands[curr_player])
    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        current_move_value = double_dummy_solver3(
            card1=card1,
            card2=card2,
            card3=card,
            hands=tuple(new_hands),
            curr_player=(curr_player + 1) % 4,
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

@functools.lru_cache(maxsize=None)
def double_dummy_solver3(
  card1: int,
  card2: int,
  card3: int,
  hands: tuple[int],
  curr_player: int,
  use_alpha_beta: bool,
  alpha: int = -999,
  beta: int = 999,
) -> int:
    is_maximizing_player = (curr_player % 2 == 0)
    best_score = -999 if is_maximizing_player else 999
    playable_cards = get_playable_cards3(card1, card2, card3, hands[curr_player])
    assert playable_cards != 0
    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        current_move_value: int
        winner_idx_in_trick = trick_winner(card1, card2, card3, card)
        winner_player = (curr_player + winner_idx_in_trick + 1) % 4
        points = get_trick_points(card1, card2, card3, card)
        points_this_trick = 0
        if winner_player % 2 == 0:
            points_this_trick = points
        new_alpha, new_beta = alpha, beta
        new_alpha = alpha - points_this_trick
        new_beta = beta - points_this_trick
        sub_game_value = double_dummy_solver0(
            hands=tuple(new_hands),
            curr_player=winner_player,
            use_alpha_beta=use_alpha_beta,
            alpha=new_alpha,
            beta=new_beta,
        )
        current_move_value = points_this_trick + sub_game_value
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
