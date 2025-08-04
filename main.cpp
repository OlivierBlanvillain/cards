#include "simulation.h"
#include <iostream>
#include <string>
#include <stdexcept>

int main(int argc, char* argv[]) {
    int iterations = 100; // Default iterations
    jass::hand_t hand = 0; // Default to random hand

    if (argc > 1) {
        try {
            iterations = std::stoi(argv[1]);
        } catch (const std::invalid_argument& ia) {
            std::cerr << "Invalid number of iterations: " << ia.what() << std::endl;
            return 1;
        } catch (const std::out_of_range& oor) {
            std::cerr << "Number of iterations out of range: " << oor.what() << std::endl;
            return 1;
        }
    }

    if (argc > 2) {
        try {
            hand = simulation::c(argv[2]);
        } catch (const std::out_of_range& oor) {
            std::cerr << "Invalid hand string: " << oor.what() << std::endl;
            return 1;
        }
    }

    simulation::run(iterations, hand);
    return 0;
}