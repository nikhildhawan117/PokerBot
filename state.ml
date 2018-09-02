open Card
open Dealer

type player = {id: string; hole_hand: card list; money: int;
              pot_money: int; is_active: bool; has_raised:bool; is_AI: bool; 
              last_cmd: string option}

type state = {round_number: int; deal_number: int; deck: deck; b_blind:int;
            s_blind:int; players: player list;
            curr_bet: int; pot:int; comm_cards: comm_cards}
