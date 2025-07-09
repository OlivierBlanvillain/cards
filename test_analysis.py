import unittest
from analysis import pretty_print_cards, iter_bits
from utils import c

class TestAnalysis(unittest.TestCase):

    def test_iter_bits(self):
        self.assertEqual(list(iter_bits(0b1011)), [0b1, 0b10, 0b1000])

    def test_pretty_print_cards(self):
        self.assertEqual(pretty_print_cards(c('7♣,9♣')), "9♣, 7♣")

if __name__ == '__main__':
    unittest.main()
