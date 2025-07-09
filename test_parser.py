import unittest
import os
from collections import defaultdict

# Import the function to be tested
from parser import parse_game_log

class TestGameParser(unittest.TestCase):

    def setUp(self):
        """Set up a temporary test file before each test."""
        self.temp_log_filename = "test_game_log.txt"
        log_content = """
P1 bids 82 1 (spade)
P2 passes
0P1 plays 8 club
0P2 plays J club
0P3 plays K club
0P4 plays 9 club
0P3 wins the trick
1P3 plays 10 spade
1P4 plays 7 spade
1P1 plays J spade
1P2 plays 8 spade
        """
        with open(self.temp_log_filename, "w", encoding='utf-8') as f:
            f.write(log_content)

    def tearDown(self):
        """Remove the temporary test file after each test."""
        if os.path.exists(self.temp_log_filename):
            os.remove(self.temp_log_filename)

    def test_parsing_full_log(self):
        """Tests the parser with the complete log from the problem description."""
        # Create a file with the full log
        full_log_content = """
        P1 bids 82 1 (spade)
        P2 passes
        P3 bids 90 6 (no trump)
        P4 passes
        P1 bids 100 1 (spade)
        P2 passes
        P3 passes
        P4 passes
        P1 passes
        Everybody passes, 100 1 for P1
        0P1 plays 8 club
        0P2 plays J club
        0P3 plays K club
        0P4 plays 9 club
        0P3 wins the trick
        1P3 plays 10 spade
        1P4 plays 7 spade
        1P1 plays J spade
        1P2 plays 8 spade
        1P1 wins the trick
        2P1 plays Q spade
        2P2 plays K spade
        2P3 plays 7 club
        2P4 plays A diamond
        2P2 wins the trick
        3P2 plays 9 spade
        3P3 plays 9 heart
        3P4 plays K heart
        3P1 plays A spade
        3P2 wins the trick
        4P2 plays Q diamond
        4P3 plays 9 diamond
        4P4 plays 10 diamond
        4P1 plays J diamond
        4P4 wins the trick
        5P4 plays 7 diamond
        5P1 plays 8 heart
        5P2 plays Q heart
        5P3 plays K diamond
        5P3 wins the trick
        6P3 plays A club
        6P4 plays Q club
        6P1 plays A heart
        6P2 plays 7 heart
        6P3 wins the trick
        7P3 plays 10 club
        7P4 plays 8 diamond
        7P1 plays 10 heart
        7P2 plays J heart
        P3 wins the last trick (+10 pts)
        """
        full_log_filename = "full_test_log.txt"
        with open(full_log_filename, "w", encoding='utf-8') as f:
            f.write(full_log_content)

        # Expected data structures
        expected_hands = {
            'P1': ['8♣', 'J♠', 'Q♠', 'A♠', 'J♦', '8♥', 'A♥', '10♥'],
            'P2': ['J♣', '8♠', 'K♠', '9♠', 'Q♦', 'Q♥', '7♥', 'J♥'],
            'P3': ['K♣', '10♠', '7♣', '9♥', '9♦', 'K♦', 'A♣', '10♣'],
            'P4': ['9♣', '7♠', 'A♦', 'K♥', '10♦', '7♦', 'Q♣', '8♦']
        }

        expected_tricks = [
            ['8♣', 'J♣', 'K♣', '9♣'],
            ['10♠', '7♠', 'J♠', '8♠'],
            ['Q♠', 'K♠', '7♣', 'A♦'],
            ['9♠', '9♥', 'K♥', 'A♠'],
            ['Q♦', '9♦', '10♦', 'J♦'],
            ['7♦', '8♥', 'Q♥', 'K♦'],
            ['A♣', 'Q♣', 'A♥', '7♥'],
            ['10♣', '8♦', '10♥', 'J♥']
        ]

        # Run parser and check results
        hands, tricks = parse_game_log(full_log_filename)
        self.assertEqual(dict(hands), expected_hands)
        self.assertEqual(tricks, expected_tricks)

        # Clean up the full log file
        os.remove(full_log_filename)

    def test_parsing_partial_log(self):
        """Tests the parser with the smaller, partial log."""
        # Expected data for the partial log in setUp
        expected_hands = defaultdict(list)
        expected_hands['P1'] = ['8♣', 'J♠']
        expected_hands['P2'] = ['J♣', '8♠']
        expected_hands['P3'] = ['K♣', '10♠']
        expected_hands['P4'] = ['9♣', '7♠']

        expected_tricks = [
            ['8♣', 'J♣', 'K♣', '9♣'],
            ['10♠', '7♠', 'J♠', '8♠']
        ]

        hands, tricks = parse_game_log(self.temp_log_filename)

        # Assert that the parsed data matches the expected data
        self.assertEqual(dict(hands), expected_hands)
        self.assertEqual(tricks, expected_tricks)

    def test_file_not_found(self):
        """Tests the function's behavior for a non-existent file."""
        hands, tricks = parse_game_log("non_existent_file.txt")
        self.assertIsNone(hands)
        self.assertIsNone(tricks)


if __name__ == '__main__':
    unittest.main(argv=['first-arg-is-ignored'], exit=False)
