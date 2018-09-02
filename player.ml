open Card
open Dealer
open State

exception Illegal

let adjust_money p num =
  {p with money = p.money + num}

let valid_bet p num =
  if num < 0 then false
  else if num > p.money then false else true

let make_player id ai deck =
	{id = id; hole_hand = deal_hole deck; money = 5000; pot_money = 0;
	is_active = true; has_raised = false; is_AI = ai; last_cmd = None}

