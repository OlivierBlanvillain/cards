# === SUIT CONSTANTS ===
C = 0x000000FF  # ♣
D = 0x0000FF00  # ♦
H = 0x00FF0000  # ♥
S = 0xFF000000  # ♠

# === POINT VALUES ===
POINTS_TABLE = [
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 0, 2, 3, 4, 10, 11,
    0, 0, 3, 4, 10, 11, 14, 20,
]

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
    assert False

def trick_winner(trick: list[int]) -> int:
    dominant = suit_of(trick[0])
    mask = sum(trick)
    candidates = mask & (S | dominant)
    winner_mask = 1 << (candidates.bit_length() - 1)
    for i, card in enumerate(trick):
        if card == winner_mask:
            return i
    assert False

def get_playable_cards(trick: list[int], hand: int) -> int:
    if not trick: return hand
    dominant = suit_of(trick[0])
    trick_mask = sum(trick)
    winner_index = trick_winner(trick)
    partner_is_leading = (winner_index == len(trick) - 2)

    has_dominant = hand & dominant
    trumps_in_hand = hand & S
    trumps_in_trick = trick_mask & S
    overtrump = 0
    if trumps_in_trick:
        overtrump = trumps_in_hand & ~((1 << trumps_in_trick.bit_length()) - 1)

    if dominant != S:
        if has_dominant: return has_dominant
        if not partner_is_leading:
            if overtrump: return overtrump
            if trumps_in_hand: return trumps_in_hand
    else:
        if overtrump: return overtrump
        if trumps_in_hand: return trumps_in_hand
    return hand

# === SOLVER ===
def solve_dd_minimax(trick, hands, turn) -> tuple[int, list[tuple[int, int]]]:
    if all(h == 0 for h in hands):
        return 0, []

    memo_key = (tuple(trick), tuple(hands), turn)
    if memo_key in solve_dd_minimax.cache:
        return solve_dd_minimax.cache[memo_key]

    legal = get_playable_cards(trick, hands[turn])
    best_path = None
    best_score = -9999

    for card in iter_bits(legal):
        new_hands = list(hands)
        new_hands[turn] ^= card
        new_trick = trick + [card]

        if len(new_trick) == 4:
            winner = (trick_winner(new_trick) + turn + 1) % 4
            pts = sum(POINTS_TABLE[b.bit_length() - 1] for b in new_trick)
            a_pts, subpath = solve_dd_minimax([], new_hands, winner)

            if winner % 2 == 0:
                a_pts += pts
        else:
            a_pts, subpath = solve_dd_minimax(new_trick, new_hands,
(turn + 1) % 4)

        my_score = a_pts if turn % 2 == 0 else -a_pts
        if my_score > best_score:
            best_score = my_score
            best_path = [(turn, card)] + subpath
            best_result = (a_pts, best_path)

    solve_dd_minimax.cache[memo_key] = best_result
    return best_result

solve_dd_minimax.cache = {}
