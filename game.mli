open Card
open State
open Ranker
open Player
open Dealer
open AI

(* [Illegal] is raised by [do'] when the player gives an improper command. *)
exception Illegal

(* [QuitGame] is raised by [game_loop] when the player enters the Quit command.*)
exception QuitGame

(* Basic commands that can be issued by any player or AI (AI cannot cashout)
that will modify the state*)
type command = Bet of int | Fold | Check | Raise of int | Call | Quit | Help

(* Prints the state for the user, such that they are aware of the current
players still in the round and their current bets *)
val print_state: state -> unit

(* Creates a 'default' state with uniform starting amounts for players and
low and high blinds such that the game can progress*)
val new_state: player list -> deck -> state

(* Determines the maximum allowed bid based on the amount of money each player
has left as per the rules of poker. For instance, if a player has $5 left,
no other player can make a bet exceeding $5 if the player with $5 wishes to stay
in the round *)
val max_bid: state -> int

(*Takes in a command and performs the command on the game state, for instance
Bet(5) would return update the state with the bet of 5 from the current player,
Fold would remove the player from contention during that game*)
val do' : command -> player -> state -> state

(*This game_loop is the repl for the game, it takes in a state and repeatedly
loops the state displaying changes due to commands from each player and
AI to the user playing the game*)
val game_loop: state -> unit

(*initiates game_loop*)
val main: unit -> unit
