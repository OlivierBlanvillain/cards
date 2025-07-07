import re
from dataclasses import dataclass, field
from enum import Enum


class Suite(Enum):
    S = "♠"
    H = "♥"
    C = "♣"
    D = "♦"
    TA = "TA"
    SA = "SA"


@dataclass
class Card:
    value: str
    suite: Suite

    def __str__(self):
        return f"{self.value}{self.suite.value}"


@dataclass
class Hand:
    cards: list[Card] = field(default_factory=list)

    def __str__(self):
        suites = {s: [] for s in Suite}
        for card in self.cards:
            suites[card.suite].append(card.value)

        s = " ".join(suites[Suite.S])
        h = " ".join(suites[Suite.H])
        c = " ".join(suites[Suite.C])
        d = " ".join(suites[Suite.D])

        m = max(len(s), len(h), len(c), len(d))

        return (
            f"╔══{'═' * m}═╗\n"
            f"║ ♠ {s.ljust(m)} ║\n"
            f"║ ♥ {h.ljust(m)} ║\n"
            f"║ ♣ {c.ljust(m)} ║\n"
            f"║ ♦ {d.ljust(m)} ║\n"
            f"╚══{'═' * m}═╝"
        )


class BidType(Enum):
    PASS = "Pass"
    BID = "Bid"
    CONTRE = "Contre"
    SURCONTRE = "Surcontre"
    CAPOT = "Capot"


@dataclass
class Bid:
    type: BidType
    value: int | None = None
    suite: Suite | None = None

    def __str__(self):
        if self.type == BidType.PASS:
            return "passes"
        elif self.type == BidType.CONTRE:
            return "doubles"
        elif self.type == BidType.SURCONTRE:
            return "redoubles"
        elif self.type == BidType.CAPOT:
            return "capot"
        else:
            return f"bids {self.value} {self.suite.value}"


@dataclass
class Game:
    h1: Hand
    h2: Hand
    h3: Hand
    h4: Hand
    bids: list[Bid]
    path: str

    @staticmethod
    def read(path: str):
        h1, h2, h3, h4 = Hand(), Hand(), Hand(), Hand()
        bids = []

        card_values = {"A": "A", "10": "10", "K": "K", "Q": "Q", "J": "J", "9": "9", "8": "8", "7": "7"}
        suite_map = {"spade": Suite.S, "heart": Suite.H, "club": Suite.C, "diamond": Suite.D}
        bid_suite_map = {'1': Suite.S, '2': Suite.H, '3': Suite.C, '4': Suite.D, '5': Suite.TA, '6': Suite.SA}

        with open(path) as f:
            for line in f:
                line = line.strip()
                if m := re.match(r"P\d passes", line):
                    bids.append(Bid(BidType.PASS))
                elif m := re.match(r"P\d bids (\d+) (\d) ([a-z ]+)", line):
                    bids.append(Bid(BidType.BID, int(m.group(1)), bid_suite_map[m.group(2)]))
                elif m := re.match(r"\dP(\d) plays (..?) (spade|heart|club|diamond)", line):
                    player = int(m.group(1))
                    card_val = card_values.get(m.group(2))
                    suite = suite_map.get(m.group(3))
                    if card_val and suite:
                        card = Card(card_val, suite)
                        if player == 1:
                            h1.cards.append(card)
                        elif player == 2:
                            h2.cards.append(card)
                        elif player == 3:
                            h3.cards.append(card)
                        elif player == 4:
                            h4.cards.append(card)
                elif re.match(r"P\d doubles!", line):
                    bids.append(Bid(BidType.CONTRE))
                elif re.match(r"P\d redoubles!", line):
                    bids.append(Bid(BidType.SURCONTRE))

        return Game(h1, h2, h3, h4, bids, path)
