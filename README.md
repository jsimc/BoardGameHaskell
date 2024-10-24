# Functional TicTacToe Game in Haskell

This project is a purely functional implementation of the classic **TicTacToe** game, written in **Haskell**. The game leverages key functional programming concepts such as **monads** and **functors** for managing game state and implementing game logic in an elegant, declarative style.

## Project Overview

- **Purely Functional**: The game is implemented using functional programming paradigms, ensuring immutability and side-effect-free computations.
- **Monads and Functors**: Used extensively for managing game state transitions and encapsulating computations.
- **Turn-Based Game**: Two players take turns to mark the grid, with the goal of getting three of their symbols in a row, column, or diagonal.
- **CLI-Based**: The game runs in the command-line interface, with simple prompts for player input.

## Key Features

- **Functional Design**: The entire game is modeled functionally, with no mutable state or side effects.
- **Monad-Based Game State**: The use of monads ensures clean handling of state transitions as the game progresses.
- **Functor-Based Logic**: Functors are utilized to map game logic transformations in a modular and reusable manner.
- **Player Input**: Players can input their moves interactively, and the game checks for valid moves and win conditions.
- **Victory Detection**: The game automatically detects when a player has won or if the game is a draw.

## Game Rules

1. The game is played on a 3x3 grid.
2. Two players, Player X and Player O, take turns marking empty spaces on the grid.
3. The first player to get three of their marks in a horizontal, vertical, or diagonal row wins.
4. If the grid is full and no player has won, the game ends in a draw.
