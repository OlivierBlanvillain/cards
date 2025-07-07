import os
from game import Game, BidType
import cards


def get_bidding_team(bids):
    last_bid = None
    player = -1
    for i, bid in enumerate(bids):
        if bid.type == BidType.BID:
            last_bid = bid
            player = i % 4
    return player % 2

def get_actual_score(path, bidding_team):
    with open(path) as f:
        for line in f:
            if "Bid successful" in line:
                parts = line.split(", ")[1].split(" to ")
                score1 = int(parts[0])
                score2 = int(parts[1][:-2])
                return score1 if bidding_team == 0 else score2
            elif "Bid fails" in line:
                parts = line.split(", ")[1].split(" to ")
                score1 = int(parts[0])
                score2 = int(parts[1][:-2])
                return score1 if bidding_team == 0 else score2
    return 0

def classify_game(game):
    hands_as_str = []
    for hand in [game.h1, game.h2, game.h3, game.h4]:
        card_strs = []
        for card in hand.cards:
            card_strs.append(f"{card.value}{card.suite.value}")
        hands_as_str.append(",".join(card_strs))

    hands_as_int = [cards.c(s) for s in hands_as_str]

    # Ensure hands_as_int is a tuple of tuples for memoization
    hands_as_tuple = tuple(hands_as_int)

    optimal_score = cards.solve_dd_minimax(tuple([]), hands_as_tuple, 0, 0, use_alpha_beta=True)

    bidding_team = get_bidding_team(game.bids)
    actual_score = get_actual_score(game.path, bidding_team)

    if actual_score == optimal_score:
        return "perfect play"
    elif actual_score < optimal_score:
        return "suboptimal play"
    else:
        return "superoptimal play"

if __name__ == "__main__":
    fst_dir = "/home/olivier/workspace/cards/fst"
    
    results = {
        "perfect play": 0,
        "suboptimal play": 0,
        "superoptimal play": 0,
    }

    for filename in os.listdir(fst_dir):
        path = os.path.join(fst_dir, filename)
        if not os.path.isfile(path):
            continue

        try:
            game = Game.read(path)
            # Basic validation for hands to prevent errors in cards.c
            if not game.h1.cards or not game.h2.cards or not game.h3.cards or not game.h4.cards:
                print(f"Skipping {path}: Incomplete hand data.")
                continue
            
            # Validate that cards.c can process the hand strings
            test_hands_as_str = []
            for hand in [game.h1, game.h2, game.h3, game.h4]:
                card_strs = []
                for card in hand.cards:
                    card_strs.append(f"{card.value}{card.suite.value}")
                test_hands_as_str.append(",".join(card_strs))
            
            # Attempt to convert to int to catch errors early
            try:
                [cards.c(s) for s in test_hands_as_str]
            except Exception as e:
                print(f"Skipping {path}: Error converting hand data: {e}")
                continue

            classification = classify_game(game)
            results[classification] += 1
        except Exception as e:
            print(f"Error processing {path}: {e}")

    print("\n--- Game Classification Results ---")
    for category, count in results.items():
        print(f"{category}: {count}")
