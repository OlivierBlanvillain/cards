from cards import *

# === DEBUGGING ===
RANKS_TRUMP = ['J','9','A','10','K','Q','8','7']
RANKS_PLAIN = ['A','10','K','Q','J','9','8','7']
CARD_TO_BIT = {}
BIT_TO_CARD = {}
bit = 31
for suit in [S, H, D, C]:
    ranks = RANKS_TRUMP if suit == S else RANKS_PLAIN
    for rank in ranks:
        CARD_TO_BIT[(suit, rank)] = bit
        BIT_TO_CARD[bit] = (suit, rank)
        bit -= 1

def c(desc: str) -> int:
    total = 0
    for token in desc.split(','):
        rank = token[:-1]
        suit = {"♣": C, "♦": D, "♥": H, "♠": S}[token[-1]]
        total |= 1 << CARD_TO_BIT[(suit, rank)]
    return total

def d(card: int) -> str:
    suit, rank = BIT_TO_CARD[card.bit_length() - 1]
    if card & C: return rank + "♣"
    if card & D: return rank + "♦"
    if card & H: return rank + "♥"
    if card & S: return rank + "♠"
    assert False

# === TEST ===
hands = [
    c("10♥,J♠,7♣,A♦"),
    c("J♥,Q♣,K♠,8♦"),
    c("9♠,A♥,7♥,10♣"),
    c("Q♠,A♣,8♠,10♦")
]
a_score, path = solve_dd_minimax([], hands, 0)
assert a_score == 94

trick = []
turn = 0
points_a = 0
points_b = 0
score, path = solve_dd_minimax([], hands, 0)
print(f"Total Team A score: {score}")
print("\nBest line of play with scores:")

hands_copy = hands[:]
trick = []
for i, (player, card) in enumerate(path):
    suit, rank = BIT_TO_CARD[card.bit_length() - 1]
    s = {C: "♣", D: "♦", H: "♥", S: "♠"}[suit]
    print(f"Player {player} plays {rank}{s}")
    hands_copy[player] ^= card
    trick.append(card)

    if len(trick) == 4:
        winner = trick_winner(trick)
        pts = sum(POINTS_TABLE[b.bit_length() - 1] for b in trick)
        if winner % 2 == 0:
            points_a += pts
        else:
            points_b += pts
        print(f"  → Trick won by Player {winner}, Points: {pts}")
        print(f"  → Team A score: {points_a}")
        trick = []

print(f"\nFinal: Team A = {points_a}, Team B = {points_b}")
# assert score == points_a, f"Mismatch: returned {score}, but traced {points_a}"


assert c("J♠") > c("9♠") > c("A♠")
assert c("7♣") == 1
assert c("J♠") == 1 << 31
print("✅ Card ordering test passed")

assert get_playable_cards([], c("7♦,K♦,A♣")) == c("7♦,K♦,A♣")
assert get_playable_cards([c("K♦")], c("J♠,9♠")) == c("J♠,9♠")
assert get_playable_cards([c("9♠")], c("8♠,J♠")) == c("J♠")
assert get_playable_cards([c("A♥")], c("7♣,Q♣")) == c("7♣,Q♣")
assert get_playable_cards([], (1 << 32) - 1) == (1 << 32) - 1
print("✅ Basic playability tests passed")

assert get_playable_cards([c("A♦"), c("7♦")], c("K♦,A♣")) == c("K♦")
assert get_playable_cards([c("A♦"), c("7♣"), c("10♥")], c("J♥,9♥")) == c("J♥,9♥")
assert get_playable_cards([c("9♠"), c("7♠")], c("J♠,8♠")) == c("J♠")
print("✅ Multi-card trick tests passed")

assert get_playable_cards([c("A♥")], c("J♠,7♠,9♣")) == c("J♠,7♠")
assert get_playable_cards([c("A♥")], c("J♠,7♠,10♥")) == c("10♥")
print("✅ Forced trump / follow tests passed")

assert POINTS_TABLE[c("A♣").bit_length() - 1] == 11
assert POINTS_TABLE[c("10♣").bit_length() - 1] == 10
assert POINTS_TABLE[c("K♣").bit_length() - 1] == 4
assert POINTS_TABLE[c("Q♣").bit_length() - 1] == 3
assert POINTS_TABLE[c("J♣").bit_length() - 1] == 2
assert POINTS_TABLE[c("9♣,8♣,7♣").bit_length() - 1] == 0
assert POINTS_TABLE[c("J♠").bit_length() - 1] == 20
assert POINTS_TABLE[c("9♠").bit_length() - 1] == 14
assert sum(POINTS_TABLE) == 152
print("✅ Point counting test passed")

assert trick_winner([c("10♦"), c("K♦"), c("A♦")]) == 2
assert trick_winner([c("10♦"), c("7♣"), c("K♦")]) == 0
print("✅ trick_winner player index tests passed")
