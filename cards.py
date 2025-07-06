C = 0x000000FF  # Clubs
D = 0x0000FF00  # Diamonds
H = 0x00FF0000  # Hearts
S = 0xFF000000  # Spades (trump suit)

POINTS_TABLE = [
    # 7, 8, 9, J,  Q,  K,  10, A
    0, 0, 0, 2, 3, 4, 10, 11,  # Clubs
    0, 0, 0, 2, 3, 4, 10, 11,  # Diamonds
    0, 0, 0, 2, 3, 4, 10, 11,  # Hearts
    # 7, 8, Q, K, 10, A, 9, J
    0, 0, 3, 4, 10, 11, 14, 20,  # Spades
]

def get_points(card: int) -> int:
    return POINTS_TABLE[card.bit_length() - 1]

def get_trick_points(trick: list[int]) -> int:
    return sum(get_points(card) for card in trick)

def iter_bits(mask: int):
    while mask:
        b = mask & -mask
        yield b
        mask ^= b

def suit_of(card: int) -> int:
    if card & C: return C
    if card & D: return D
    if card & H: return H
    if card & S: return S
    raise ValueError("Invalid card value")

def trick_winner(trick: list[int]) -> int:
    led_suit = suit_of(trick[0])
    trick_mask = sum(trick)
    potential_winners = trick_mask & (S | led_suit)
    winning_card_mask = 1 << (potential_winners.bit_length() - 1)
    for i, card in enumerate(trick):
        if card == winning_card_mask:
            return i
    assert False, "unreachable"


def get_playable_cards(trick: list[int], hand: int) -> int:
    if not trick:
        return hand

    led_suit = suit_of(trick[0])
    trick_mask = sum(trick)
    trumps_in_trick = trick_mask & S
    if trumps_in_trick:
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
           hand = (hand & ~S) | overtrumps

    cards_in_led_suit = hand & led_suit
    if cards_in_led_suit:
        return cards_in_led_suit

    trumps_in_hand = hand & S
    if not trumps_in_hand:
        return hand

    current_winner_index = trick_winner(trick)
    partner_is_winning = (current_winner_index == (len(trick) - 2))
    if partner_is_winning:
        return hand

    return trumps_in_hand


def solve_dd_minimax(
  trick: list[int], hands: list[int], curr_player: int, leading_player: int
) -> tuple[int, list[tuple[int, int]]]:
    if all(h == 0 for h in hands):
        return 0, []

    playable_cards = get_playable_cards(trick, hands[curr_player])
    best_path = []
    best_score = -9999
    best_result = (0, [])

    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[curr_player] ^= card
        new_trick = trick + [card]

        score_for_team_a = 0
        sub_path = []

        is_trick_over = len(new_trick) == 4
        if is_trick_over:
            winner_player = (leading_player + trick_winner(new_trick)) % 4
            points = get_trick_points(new_trick)
            score_for_team_a, sub_path = solve_dd_minimax([], new_hands, winner_player, winner_player)
            if winner_player % 2 == 0:  # Team A wins trick
                score_for_team_a += points
        else:
            next_player = (curr_player + 1) % 4
            score_for_team_a, sub_path = solve_dd_minimax(new_trick, new_hands, next_player, leading_player)

        current_player_score = score_for_team_a if curr_player % 2 == 0 else -score_for_team_a

        if current_player_score > best_score:
            best_score = current_player_score
            best_path = [(curr_player, card)] + sub_path
            best_result = (score_for_team_a, best_path)

    return best_result
