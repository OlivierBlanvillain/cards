import re

def should_pass(hand: list[str]) -> bool:
    PASS = True
    CALL = False
    jack_cards = [card for card in hand if card.startswith("J")]
    jack_count = len(jack_cards)

    suits = ['S', 'H', 'D', 'C']

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

    trump_options = []
    for suit in suits:
        trumps = [card.removesuffix(suit) for card in hand if card.endswith(suit)]
        trump_options.append(trumps)

    def trump_matches(predicate):
        return any(map(predicate, trump_options))

    # 4xJ -> pass
    if jack_count == 4:
        return PASS

    # J 9 3th -> call
    if trump_matches(lambda trumps: 'J' in trumps and '9' in trumps and len(trumps) >= 3):
        return CALL

    # 4 specials -> pass
    if specials == 4:
        return PASS

    # J9 -> call
    if trump_matches(lambda trumps: 'J' in trumps and '9' in trumps):
        return CALL

    # j4rd -> call
    if trump_matches(lambda trumps: 'J' in trumps and len(trumps) >= 4):
        return CALL

    # JKQ -> call
    if trump_matches(lambda trumps: 'J' in trumps and 'K' in trumps and 'Q' in trumps):
        return CALL

    # 3 specials -> pass
    if specials >= 3:
        return PASS

    # 96th -> call
    if trump_matches(lambda trumps: '9' in trumps and len(trumps) >= 6):
        return CALL

    # 9QK4th -> call
    if trump_matches(lambda trumps: '9' in trumps and 'K' in trumps and 'Q' in trumps and len(trumps) >= 4):
        return CALL

    # # 2 specials -> pass
    # if specials >= 2:
    #     return PASS

    # j3rd -> call
    if trump_matches(lambda trumps: 'J' in trumps and len(trumps) >= 3):
        return CALL

    return PASS

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
        ev_diff = abs(best_ev - ev_values[sorted(ev_values, key=ev_values.get)[-2]])
        print(f"ev_diff={ev_diff:05.2f} \t '{file_path[:16]}' \t system {'passes' if system_passes else 'calls'} \t hand={hand_str}")

# Example usage
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python parse_ev.py <file_path>")
    else:
        parse_hand_and_best_move(sys.argv[1])
