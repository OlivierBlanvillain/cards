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

    optimal_score = cards.solve_dd_minimax([], hands_as_int, 0, 0, use_alpha_beta=True)

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
    for filename in os.listdir(fst_dir):
        if os.path.isfile(os.path.join(fst_dir, filename)):
            path = os.path.join(fst_dir, filename)
            try:
                game = Game.read(path)
                if not game.h1.cards or not game.h2.cards or not game.h3.cards or not game.h4.cards:
                    continue
                classification = classify_game(game)
                print(f"{path}: {classification}")
            except Exception as e:
                print(f"Error processing {path}: {e}")
