import re
import sys
from collections import defaultdict

def parse_game_log(filepath):
    """
    Parses a game log from a file to extract hands and tricks.

    Args:
        filepath (str): The path to the game log file.

    Returns:
        A tuple containing two elements:
        - A dictionary of player hands (e.g., {'P1': ['8♣', ...]})
        - A list of lists, where each inner list represents a trick.
    """
    suit_map = {
        'club': '♣', 'spade': '♠', 'diamond': '♦', 'heart': '♥'
    }
    hands = defaultdict(list)
    tricks = []

    # Regex to find lines describing a play
    # Captures: 1. Trick #, 2. Player ID, 3. Card Rank, 4. Card Suit
    play_pattern = re.compile(r"^(\d)(P\d) plays (\S+) (\S+)")

    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            for line in f:
                match = play_pattern.match(line.strip())
                if match:
                    trick_num_str, player_id, card_rank, card_suit = match.groups()
                    trick_num = int(trick_num_str)

                    card = card_rank + suit_map[card_suit]

                    # Ensure the tricks list is long enough
                    while len(tricks) <= trick_num:
                        tricks.append([])

                    hands[player_id].append(card)
                    tricks[trick_num].append(card)

    except FileNotFoundError:
        print(f"Error: The file '{filepath}' was not found.", file=sys.stderr)
        return None, None
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        return None, None

    return hands, tricks

def format_and_print_output(hands, tricks):
    """
    Formats the parsed data into the required output string format and prints it.
    """
    # --- Print Hands ---
    print("hands = [")
    for i in range(1, 5):
        player_id = f"P{i}"
        hand_str = ",".join(hands.get(player_id, []))
        print(f'  c("{hand_str}"),')
    print("]")
    print()  # Blank line for separation

    # --- Print Tricks (with commas) ---
    print("tricks = [")
    num_tricks = len(tricks)
    for i, trick in enumerate(tricks):
        trick_str = ",".join(trick)
        # Add a comma to all but the last line
        comma = "," if i < num_tricks - 1 else ""
        print(f'  c("{trick_str}"){comma}')
    print("]")

# This block allows the script to be run from the command line
if __name__ == "__main__":
    # Expecting one argument: the script name and the filename
    if len(sys.argv) != 2:
        print("Usage: python game_parser.py <path_to_log_file>")
        sys.exit(1)

    input_filepath = sys.argv[1]
    parsed_hands, parsed_tricks = parse_game_log(input_filepath)

    # Only print if parsing was successful
    if parsed_hands and parsed_tricks:
        format_and_print_output(parsed_hands, parsed_tricks)
