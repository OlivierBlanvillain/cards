from belote import C, D, H, S

RANKS_TRUMP = ['J', '9', 'A', '10', 'K', 'Q', '8', '7']
RANKS_PLAIN = ['A', '10', 'K', 'Q', 'J', '9', '8', '7']
CARD_TO_BIT = {}
BIT_TO_CARD = {}

bit = 31
for suit, suit_name in [(S, "♠"), (H, "♥"), (D, "♦"), (C, "♣")]:
    ranks = RANKS_TRUMP if suit == S else RANKS_PLAIN
    for rank in ranks:
        CARD_TO_BIT[(suit, rank)] = bit
        BIT_TO_CARD[bit] = (suit, rank, suit_name)
        bit -= 1

def c(desc: str) -> int:
    """Converts a comma-separated string of cards (e.g., 'A♠,K♥') to a bitmask."""
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
    """Converts a single card bitmask back to its string representation (e.g., 'A♠')."""
    if card_mask == 0:
        return ""
    bit_pos = card_mask.bit_length() - 1
    suit, rank, suit_char = BIT_TO_CARD[bit_pos]
    return rank + suit_char

def pretty_print_hand(hand: int) -> str:
    return ", ".join(reversed([d(card) for card in iter_bits(hand)]))

def iter_bits(mask: int):
    while mask:
        b = mask & -mask
        yield b
        mask ^= b
