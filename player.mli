open Card
open Dealer
open State

(*Adds or subtracts a certain amount of money from the player's current total
as a result of winning or losing a round, or betting. *)
val adjust_money: player -> int -> player

(*Checks whether a bet that a player is trying to make is valid or not*)
val valid_bet: player -> int -> bool

(* Creates a player, given its id, whether or not it is an AI, and a deck
 * for the game. *)
val make_player: string -> bool -> deck -> player



