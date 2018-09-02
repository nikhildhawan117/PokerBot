open Card
open Dealer
open State

exception Tie
exception Illegal

(* List of tules mapping each player to the cards that they can use to amek a
hand.

AF: This data structure contains the list of every mapping of a  player's id
to their cards.

RI: No cards in the list can be repeated, they must all be unique*)
type player_cards = (string * card list) list

(* Takes in the current state of the game and returns the player_cards that each
player can use to make a hand *)
val make_player_cards: state -> player_cards

(* Returns the winner of the round if every player must show their hands
(all bets are place, river has gone down) based on who has highest ranked hand
as per the rules of holdem*)
val winner: player_cards -> string
