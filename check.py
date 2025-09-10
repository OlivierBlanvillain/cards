import re

def should_pass(hand: list[str]) -> bool:
    suits = ['S', 'H', 'D', 'C']

    for suit in suits:
        trump_cards = [card for card in hand if card.endswith(suit)]
        trump_count = len(trump_cards)

        has_jack = f"J{suit}" in hand
        has_nine = f"9{suit}" in hand

        if has_jack and has_nine and trump_count >= 4:
            return False

    specials = 0
    for suit in suits:
        has_jack = f"J{suit}" in hand
        has_nine = f"9{suit}" in hand
        has_king = f"K{suit}" in hand
        has_queen = f"Q{suit}" in hand

        if has_jack:
            specials += 1
        elif has_nine:
            specials += 1
        elif has_king and has_queen:
            specials += 1

    if specials == 4:
        return True

    for suit in suits:
        trump_cards = [card for card in hand if card.endswith(suit)]
        trump_count = len(trump_cards)

        has_jack = f"J{suit}" in hand
        has_nine = f"9{suit}" in hand
        has_king = f"K{suit}" in hand
        has_queen = f"Q{suit}" in hand

        if has_jack and has_nine and trump_count >= 3:
            return False
        if has_jack and trump_count >= 4:
            return False
        if has_jack and has_king and has_queen:
            return False

    if specials >= 3:
        return True

    for suit in suits:
        trump_cards = [card for card in hand if card.endswith(suit)]
        trump_count = len(trump_cards)

        has_jack = f"J{suit}" in hand
        has_nine = f"9{suit}" in hand
        has_king = f"K{suit}" in hand
        has_queen = f"Q{suit}" in hand

        if has_jack and has_nine:
            return False
        if has_jack and trump_count >= 3:
            return False
        if has_nine and trump_count >= 6:
            return False
        if has_nine and has_king and has_queen and trump_count >= 4:
            return False

    return True

def parse_hand_and_best_move(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()[-7:]

    # Parse the hand from the first line
    hand_line = lines[0].strip()
    hand_str = hand_line.split('(')[0].strip()  # Remove the (1000x) part
    hand = [card.strip() for card in hand_str.split(',')]

    system_passes = should_pass(hand)

    # Dictionary to store EV values
    ev_values = {}

    # Regex pattern to match suit and EV
    ev_pattern = r'^([SHDCP])\s*:\s*([\d.]+)'

    # Parse EV lines
    for line in lines[1:]:
        match = re.match(ev_pattern, line.strip())
        if match:
            suit = match.group(1)
            ev = float(match.group(2))
            ev_values[suit] = ev

    # Find the suit with the highest EV
    best_suit = max(ev_values, key=ev_values.get)
    best_ev = ev_values[best_suit]
    ai_passes = (best_suit == "P")
    if system_passes != ai_passes:
        print(f"disagree on '{file_path}', {system_passes=}, {ai_passes=}, hand={hand_str}, {ev_values=}")

# Example usage
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python parse_ev.py <file_path>")
    else:
        parse_hand_and_best_move(sys.argv[1])
