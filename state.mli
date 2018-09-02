open Card
open Dealer

(*Player definition holding the player's id, current hand, remaining money,
whether or not the player is active in the round and whether the player
is an AI or a user controlled player the last command the player issued
the amount of money the player has in the pot and whether or not the
player has raised. This is all to keep track of the player's status and funds
in the course of the game. The same type is used for the AI. I.e both AI and
human are of the type player*)
type player = {id: string; hole_hand: card list; money: int;
              pot_money: int; is_active: bool; has_raised:bool; is_AI: bool;
              last_cmd: string option}

(* Definition of the state of a game. This contains the round the game is on,
which deal it is, the deck of cards being used, the blinds, the current bet
that the players must be at in order to be active in the game, the list of
players, the pot amount and the community cards all the players have access to
in order to build hands.*)
type state = {round_number: int; deal_number: int; deck: deck; b_blind:int;
            s_blind:int; players: player list;
            curr_bet: int; pot:int; comm_cards: comm_cards}
