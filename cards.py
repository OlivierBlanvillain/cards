# SUIT AND CARD REPRESENTATION
# ============================
# The card game logic uses a bitmask representation for cards and hands.
# Each of the 32 cards in the deck is assigned a unique bit.
# A player's hand is represented by an integer where the set bits correspond to the cards they hold.
# Suits are represented by masks that cover the bits for all cards of that suit.

# === SUIT MASKS ===
C = 0x000000FF  # Clubs (♣)
D = 0x0000FF00  # Diamonds (♦)
H = 0x00FF0000  # Hearts (♥)
S = 0xFF000000  # Spades (♠, trump suit)

# === POINT VALUES ===
# The points for each card are stored in a lookup table.
# The index corresponds to the card's bit position (0-31).
POINTS_TABLE = [
    # Plain Suits (Clubs, Diamonds, Hearts)
    # 7, 8, 9, J,  Q,  K,  10, A
    0, 0, 0, 2, 3, 4, 10, 11,  # Clubs
    0, 0, 0, 2, 3, 4, 10, 11,  # Diamonds
    0, 0, 0, 2, 3, 4, 10, 11,  # Hearts
    # Trump Suit (Spades)
    # 7, 8, Q, K, 10, A, 9, J
    0, 0, 3, 4, 10, 11, 14, 20,  # Spades
]

def get_points(card: int) -> int:
    """Returns the point value of a single card."""
    if card == 0:
        return 0
    # The bit_length() of a power of 2 is its exponent + 1.
    # e.g., (1 << 5).bit_length() is 6. We subtract 1 for a 0-based index.
    return POINTS_TABLE[card.bit_length() - 1]

def get_trick_points(trick: list[int]) -> int:
    """Calculates the total point value of a completed trick."""
    return sum(get_points(card) for card in trick)

def iter_bits(mask: int):
    """Yields each individual bit from a bitmask."""
    while mask:
        b = mask & -mask  # Extract the lowest set bit
        yield b
        mask ^= b         # Clear the lowest set bit

def suit_of(card: int) -> int:
    """Returns the suit mask for a given card."""
    if card & C: return C
    if card & D: return D
    if card & H: return H
    if card & S: return S
    raise ValueError("Invalid card value")

def trick_winner(trick: list[int]) -> int:
    """
    Determines the index of the winning card in a trick.

    Args:
        trick: A list of cards played in the trick.

    Returns:
        The index of the winning card in the trick list.
    """
    if not trick:
        raise ValueError("Cannot determine winner of an empty trick")

    led_suit = suit_of(trick[0])
    trick_mask = sum(trick)

    # A card can win if it's a trump or if it follows the led suit.
    potential_winners = trick_mask & (S | led_suit)

    # The highest bit in the potential_winners mask corresponds to the highest-ranking card.
    # Find the highest set bit, which represents the winning card.
    winning_card_mask = 1 << (potential_winners.bit_length() - 1)

    for i, card in enumerate(trick):
        if card == winning_card_mask:
            return i

    raise RuntimeError("Could not determine trick winner")


def get_playable_cards(trick: list[int], hand: int) -> int:
    """
    Determines which cards from a hand are legal to play in the current trick.
    This function enforces standard trick-taking rules:
    1.  You must follow the suit that was led, if possible.
    2.  If the led suit is a trump, you must play a higher trump if possible.
    3.  If you cannot follow suit, you must play a trump if you have one.
    4.  If you cannot follow suit but your partner is winning, you are not required to trump.
    5.  If you must trump and other trumps have been played, you must play a higher trump if possible.
    """
    # If leading the trick, any card is playable.
    if not trick:
        return hand

    led_suit = suit_of(trick[0])
    trick_mask = sum(trick)

    # Rule 1: Must follow suit if possible.
    cards_in_led_suit = hand & led_suit
    if cards_in_led_suit:
        # Rule 1a: If following the trump suit, must overtrump if possible.
        if led_suit == S:
            trumps_in_trick = trick_mask & S
            if trumps_in_trick:
                highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
                overtrumps = cards_in_led_suit & ~((highest_trump_in_trick << 1) - 1)
                if overtrumps:
                    return overtrumps
        # If not leading trump, or unable to overtrump, any card of the suit is legal.
        return cards_in_led_suit

    # Rule 2: Cannot follow suit. Must trump if possible.
    trumps_in_hand = hand & S
    if not trumps_in_hand:
        # No trumps and cannot follow suit, so any card is legal.
        return hand

    # Player has trumps and is void in the led suit.
    # Rule 2a: Check if partner is currently winning the trick.
    current_winner_index = trick_winner(trick)
    # The player's partner is the one who played 2 cards before them.
    partner_is_winning = (current_winner_index == (len(trick) - 2))

    # If partner is winning, player is not forced to trump and can play any card.
    if partner_is_winning:
        return hand

    # Rule 2b: Partner is not winning, so player must trump.
    trumps_in_trick = trick_mask & S
    if trumps_in_trick:
        # If other trumps are in the trick, must overtrump if possible.
        highest_trump_in_trick = 1 << (trumps_in_trick.bit_length() - 1)
        overtrumps = trumps_in_hand & ~((highest_trump_in_trick << 1) - 1)
        if overtrumps:
            return overtrumps

    # If unable to overtrump, or if no trumps were in the trick, any trump is legal.
    return trumps_in_hand


# === DOUBLE-DUMMY SOLVER ===
def solve_dd_minimax(trick: list[int], hands: list[int], turn: int, leader: int) -> tuple[int, list[tuple[int, int]]]:
    """
    Calculates the best possible score for the current player using a minimax algorithm.

    Args:
        trick: The cards played in the current trick.
        hands: A list of four hands (bitmasks).
        turn: The index of the current player (0-3).
        leader: The index of the player who led the current trick.

    Returns:
        A tuple containing:
        - The maximum score achievable by Team A (players 0 and 2).
        - The optimal line of play as a list of (player, card) tuples.
    """
    # Base case: All cards have been played.
    if all(h == 0 for h in hands):
        return 0, []

    # If the current player has no cards, skip to the next player.
    if hands[turn] == 0:
        return solve_dd_minimax(trick, hands, (turn + 1) % 4, leader)

    # Memoization to avoid re-computing the same game state.
    memo_key = (tuple(trick), tuple(hands), turn, leader)
    if memo_key in solve_dd_minimax.cache:
        return solve_dd_minimax.cache[memo_key]

    playable_cards = get_playable_cards(trick, hands[turn])
    best_path = []
    best_score = -9999
    final_result = (0, []) # Default result if no move is made

    # Iterate through all legal moves.
    for card in iter_bits(playable_cards):
        new_hands = list(hands)
        new_hands[turn] ^= card
        new_trick = trick + [card]

        score_for_team_a = 0
        next_player = (turn + 1) % 4
        sub_path = []

        # A trick is over if it has 4 cards
        is_trick_over = len(new_trick) == 4

        if is_trick_over:
            winner_player = (leader + trick_winner(new_trick)) % 4
            points = get_trick_points(new_trick)
            score_for_team_a, sub_path = solve_dd_minimax([], new_hands, winner_player, winner_player)
            if winner_player % 2 == 0:  # Team A wins trick
                score_for_team_a += points
        else:
            score_for_team_a, sub_path = solve_dd_minimax(new_trick, new_hands, next_player, leader)

        current_player_score = score_for_team_a if turn % 2 == 0 else -score_for_team_a

        if current_player_score > best_score:
            best_score = current_player_score
            best_path = [(turn, card)] + sub_path
            final_result = (score_for_team_a, best_path)

    solve_dd_minimax.cache[memo_key] = final_result
    return final_result

# Initialize the cache for the solver.
solve_dd_minimax.cache = {}
