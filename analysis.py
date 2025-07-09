from utils import RANKS_TRUMP, RANKS_PLAIN, CARD_TO_BIT, BIT_TO_CARD, c, d
from cards import (
    double_dummy_solver1,
    double_dummy_solver2,
    double_dummy_solver3,
    get_playable_cards1,
    get_playable_cards2,
    get_playable_cards3,
    trick_winner,
    get_points,
    double_dummy_solver0,
)

def iter_bits(mask: int):
    while mask:
        b = mask & -mask
        yield b
        mask ^= b

def pretty_print_cards(hand_mask: int) -> str:
    return ", ".join(reversed([d(card) for card in iter_bits(hand_mask)]))

def print_mistake_report(hands, trick, trick_leader, current_player, played_card, optimal_moves, points_lost, trick_idx, card_idx_in_trick):
    print("="*70)
    print(f"MISTAKE DETECTED on Trick {trick_idx + 1}")
    print("-"*70)
    print("Game State:")
    for i, hand_mask in enumerate(hands):
        is_current = " <- To play" if i == current_player else ""
        print(f"  Player {i}'s hand: {pretty_print_cards(hand_mask)} {is_current}")

    trick_str = ", ".join([d(card) for card in trick]) if trick else "(empty)"
    print(f"\n  Trick leader: Player {trick_leader}")
    print(f"  Current trick: [{trick_str}]")

    print("\nAnalysis:")
    print(f"  Move played        : {d(played_card)}")
    optimal_moves_str = ", ".join((d(card) for card in reversed(sorted(optimal_moves))))
    print(f"  Optimal move(s)    : {optimal_moves_str}")
    print(f"  Points lost        : {int(points_lost)}")
    print("="*70 + "\n")

def analyze_game(initial_hands_str: list[str], played_tricks_str: list[list[str]]):
    initial_hands_bit = tuple(c(h) for h in initial_hands_str)
    played_tricks_bit = [[c(card) for card in trick] for trick in played_tricks_str]

    hands_at_last_completed_trick = list(initial_hands_bit)
    trick_leader = 0

    for trick_idx, trick_cards_bit in enumerate(played_tricks_bit):
        current_trick_bit = []
        for card_idx_in_trick, played_card_bit in enumerate(trick_cards_bit):
            current_player = (trick_leader + card_idx_in_trick) % 4

            hands_for_this_turn = list(hands_at_last_completed_trick)
            for i in range(card_idx_in_trick):
                player_who_played = (trick_leader + i) % 4
                card_in_trick = trick_cards_bit[i]
                hands_for_this_turn[player_who_played] ^= card_in_trick

            hand = hands_for_this_turn[current_player]
            match current_trick_bit:
                case []:
                    playable_cards = hand
                case [c1]:
                    playable_cards = get_playable_cards1(c1, hand)
                case [c1, c2]:
                    playable_cards = get_playable_cards2(c1, c2, hand)
                case [c1, c2, c3]:
                    playable_cards = get_playable_cards3(c1, c2, c3, hand)
                case _:
                    raise ValueError()

            move_evaluations = {}
            is_maximizing_player = (current_player % 2 == 0)

            for card_to_evaluate in iter_bits(playable_cards):
                new_hands, new_trick = list(hands_for_this_turn), tuple(current_trick_bit + [card_to_evaluate])
                new_hands[current_player] ^= card_to_evaluate
                next_player = (current_player + 1) % 4
                match new_trick:
                    case (c1,):
                        value = double_dummy_solver1(
                            card1=c1,
                            hands=new_hands,
                            curr_player=next_player,
                            remaining_cards=sum(new_hands),
                            alpha=-999,
                            beta=999,
                        )
                    case (c1, c2):
                        value = double_dummy_solver2(
                            card1=c1,
                            card2=c2,
                            hands=new_hands,
                            curr_player=next_player,
                            remaining_cards=sum(new_hands),
                            alpha=-999,
                            beta=999,
                        )
                    case (c1, c2, c3):
                        value = double_dummy_solver3(
                            card1=c1,
                            card2=c2,
                            card3=c3,
                            hands=new_hands,
                            curr_player=next_player,
                            remaining_cards=sum(new_hands),
                            alpha=-999,
                            beta=999,
                        )
                    case (_, _, _, _):
                        winner_idx = trick_winner(*new_trick)
                        winner_player = (trick_leader + winner_idx) % 4
                        points = sum(map(get_points, new_trick))
                        points_for_max_player = points if (winner_player % 2 == 0) else 0
                        sub_game_value = double_dummy_solver0(
                            hands=new_hands,
                            curr_player=winner_player,
                            remaining_cards=sum(new_hands),
                            alpha=-999,
                            beta=999,
                        )
                        value = points_for_max_player + sub_game_value
                    case _:
                        raise ValueError()

                move_evaluations[card_to_evaluate] = value
            actual_move_score = move_evaluations[played_card_bit]
            optimal_score = max(move_evaluations.values()) if is_maximizing_player else min(move_evaluations.values())
            points_lost = (optimal_score - actual_move_score) if is_maximizing_player else (actual_move_score - optimal_score)

            if points_lost > 0:
                optimal_moves = [card for card, score in move_evaluations.items() if score == optimal_score]
                print_mistake_report(
                    hands_for_this_turn, current_trick_bit, trick_leader, current_player,
                    played_card_bit, optimal_moves, points_lost, trick_idx, card_idx_in_trick
                )

            current_trick_bit.append(played_card_bit)

        # After the trick is over, update the hands state for the next trick's start
        for i in range(4):
            player = (trick_leader + i) % 4
            hands_at_last_completed_trick[player] ^= trick_cards_bit[i]

        winner_idx_in_trick = trick_winner(*current_trick_bit)
        trick_leader = (trick_leader + winner_idx_in_trick) % 4

# --- Main execution ---
if __name__ == '__main__':
    game_hands = [
      "8♣,J♠,Q♠,A♠,J♦,8♥,A♥,10♥",
      "J♣,8♠,K♠,9♠,Q♦,Q♥,7♥,J♥",
      "K♣,10♠,7♣,9♥,9♦,K♦,A♣,10♣",
      "9♣,7♠,A♦,K♥,10♦,7♦,Q♣,8♦",
    ]
    game_tricks = [
      ["8♣", "J♣", "K♣", "9♣"],
      ["10♠", "7♠", "J♠", "8♠"],
      ["Q♠", "K♠", "7♣", "A♦"],
      ["9♠", "9♥", "K♥", "A♠"],
      ["Q♦", "9♦", "10♦", "J♦"],
      ["7♦", "8♥", "Q♥", "K♦"],
      ["A♣", "Q♣", "A♥", "7♥"],
      ["10♣", "8♦", "10♥", "J♥"],
    ]

    print("Analyzing Belote game for mistakes...\n")
    analyze_game(game_hands, game_tricks)
