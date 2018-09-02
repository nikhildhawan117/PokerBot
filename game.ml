
open Card
open State
open Ranker
open Player
open Dealer
open AI

type command = Bet of int | Fold | Check | Raise of int | Call | Quit | Help

(* [Illegal] is raised by [do'] when the player gives an improper command. *)
exception Illegal

(* [QuitGame] is raised by [game_loop] when the player enters the Quit command.*)
exception QuitGame

(* returns the maximum possible bet a player could make for a certain state. *)
let max_bid st =
	let p1 = List.nth st.players 0 in
	let p2 = List.nth st.players 1 in
	if st.deal_number = 1 then if p1.money < p2.money then p1.money + st.b_blind
	else p2.money + st.b_blind
	else if p1.money < p2.money then p1.money else p2.money

(* prints out information regarding the state of the game to the interface *)
let print_state st =
	print_endline ("\nCurrent bet: $" ^ (string_of_int st.curr_bet));
	if st.deal_number <> 1 then
	(print_endline ("Maximum possible bet/raise you can make: $" ^
	string_of_int (max_bid st));)
	else print_endline ("Maximum possible bet/raise you can make: $" ^
	string_of_int ((max_bid st) - st.b_blind));
	print_endline ("Pot: $" ^ (string_of_int st.pot));
	print_endline ("AI Funds: $" ^ (string_of_int (List.find
	(fun x-> x.is_AI) st.players).money)^"\n\n");
	let cards = match List.map card_to_string st.comm_cards with
		| [] -> "?? | ?? | ?? "
		| a::b::c::d::e::[] -> a ^ " | " ^ b ^ " | " ^ c ^ " | " ^ d ^ " | " ^ e
		| a::b::c::d::[] -> a ^ " | " ^ b ^ " | " ^ c ^ " | " ^ d
		| a::b::c::[] -> a ^ " | " ^ b ^ " | " ^ c
		| _ -> failwith "Unreachable" in
	let width, height = ANSITerminal.size() in
	ANSITerminal.move_cursor (width/2 - 11)(0);
	print_endline ("Community cards: ");
	print_endline ("");
	ANSITerminal.move_cursor (width/2 - 11)(0);
	print_endline (cards ^ "\n");
	match st.deal_number with
		| 1 -> print_endline "\nBetting round #1.";
		| 3 -> print_endline "\nBetting round #2.";
		| 5 -> print_endline "\nBetting round #3.";
		| 7 -> print_endline "\nFinal betting round.";
		| _ -> failwith "Unreachable"

(* creates a new state *)
let new_state players deck =
	{round_number = 1; deal_number = 0; deck = deck; b_blind = 100;
	s_blind = 50; players = players; curr_bet = 0; pot = 0;
	comm_cards = []}

(* transforms a string command into a string list of words *)
let command_lst command =
	command |> String.lowercase_ascii |> Str.split (Str.regexp "[ \t]+") |>
	List.map String.trim

(* takes the input from a player and returns the proper command type *)
let get_command command =
	let c = command_lst command in
	match c with
	| "bet"::i::[] -> (try
		let num = int_of_string i in Bet num
		with
		| _ -> raise Illegal)
	| ["fold"] -> Fold
	| ["check"] -> Check
	| ["call"] -> Call
	| "raise"::i::[] -> (try
		let num = int_of_string i in Raise num
		with
		| _ -> raise Illegal)
	| ["quit"] -> Quit
	| ["help"] -> Help
	| _ -> raise Illegal

(* helper function that updates the game state for a Bet command*)
let rec do_bet num player st =
	if num > (max_bid st) || not (valid_bet player num) then
		if player.is_AI then do_bet (max_bid st) player st
		else (print_endline ("This bet is too high.");
		raise Illegal)
	else
	let p = adjust_money player (0-num) in
	let p' = {p with pot_money = num; has_raised = true;
	last_cmd = Some "bet"} in
	let players' = List.map (fun x -> if x = player then p' else x) st.players
	in {st with players = players'; curr_bet = num; pot = st.pot + num}

(* updates the game state for a Fold command *)
let do_fold player st =
	let p = {player with is_active = false; has_raised = false; last_cmd =
	Some "fold"} in
	let players' = List.map (fun x -> if x = player then p else x) st.players in
	{st with players = players'}

(* updates the game state for a Call command *)
let do_call player st =
	let p = adjust_money player (0-st.curr_bet+player.pot_money) in
	let p' = {p with pot_money = st.curr_bet; last_cmd = Some
	"call"} in
	let players' = List.map (fun x -> if x = player then p' else x) st.players in
	{st with players = players'; pot = st.pot + st.curr_bet-player.pot_money}

(* updates the game state for a Raise command *)
let rec do_raise num player st =
	if (st.deal_number <> 1 && num > max_bid st) ||
	(st.deal_number = 1 && num > max_bid st - st.b_blind) ||
	not (valid_bet player num) then
		if player.is_AI then
			if st.curr_bet = (max_bid st) then do_raise 0 player st
			else do_raise (max_bid st - st.curr_bet) player st
		else (print_endline("This bet exceeds the maximum bet."); raise Illegal)
	else if num < 0 then (print_endline ("This bet is invalid.");
		raise Illegal)
	else
	let p = adjust_money player (0-num-st.curr_bet+player.pot_money) in
	let p' = {p with has_raised = true; pot_money = st.curr_bet + num;
	last_cmd = Some "raise"} in
	let players' = List.map (fun x -> if x = player then p' else x) st.players in
	{st with players = players'; curr_bet = num + st.curr_bet; pot = st.pot + num
	+ st.curr_bet-player.pot_money}

(* updates the game state for a Check command *)
let do_check player st =
	let p = {player with last_cmd = Some "check"} in
	let players' = List.map (fun x -> if x = player then p else x) st.players in
	{st with players = players'}

(* changes the state of the game based on a player command *)
let do' command player st =
	match command with
	| Bet(num) -> do_bet num player st
	| Fold -> do_fold player st
	| Check -> do_check player st
	| Call -> do_call player st
	| Raise(num) -> do_raise num player st
	| Quit -> st
	| Help -> st

(* the game instructions, displayed when the player gives a Help command *)
let instructions = "Two-Person Texas Hold'em Poker Rules:
	Term Definitions:
	1. Hole Cards: The two personal cards that a player has.
	2. Blinds: Forced bets of small amounts of money to initiate the game.
	3. Flop: The dealing of 3 community cards visible to all players.
	4. Turn: The dealing of a fourth card after the flop and second betting
	round.
	5. River: The dealing of a fifth and final community card after the turn
	and the third betting round.
	6. Fold: To throw away one's hand, paying nothing to the pot, and wait
	until the next round.
	7. Check: The decision not to bet (when no one else has bet either).
	8. Call: To match the bet of the last bet made.
	9. Bet: To make a bet when no one else has.
	10. Raise: To make a higher bet than the previous bet.
	Procedure:
	1. Posting of blinds: The first player will post a small blind,
	while the second posts a big blind. This will alternate for each round.
	2. Dealing of hole cards: Each player is dealt two cards that only
	they may see.
	3. The preflop (first) betting round ensues, starting with the player
	who posted the small_blind, in which he/she may call the big blind,
	raise, or fold. The other player may then raise, call, or fold. No
	person may raise twice. Play continues when each person has either
	folded or matched the last bet made.
	4. The flop is dealt.
	5. The second betting round occurs, in which players have the option of
	checking (making no bet), betting if none has been made, calling a bet,
	folding, or raising a bet already made. Again, no person may raise
	twice.
	6. The turn is dealt.
	7. The third betting round occurs.
	8. The river is dealt.
	9. The final betting round occurs.
	10. If more than one player remains, a showdown occurs in which players
	reveal their cards, and the person with the highest hand is determined.
	That winner will receive the money in the pot before the new round
	begins.
	Player Commands:
	fold, bet (number), check, raise (number), help, quit, call
	Example: If a player wanted to fold, he/she would enter 'fold',
	leaving out quotations.
	If he/she wanted to bet 20 dollars, he/she would enter 'bet 20'.\n\n"

(* Operates the game, and is responsible for handling betting and dealing *)
let rec game_loop st =

	(* posts blinds *)
	let post_blinds st =
		let p1 = adjust_money (List.nth st.players 0) (0-st.s_blind) in
		let p1' = {p1 with pot_money = st.s_blind} in
		print_endline ("\n" ^ p1.id ^ " has posted a small blind of " ^
			(string_of_int st.s_blind) ^ " dollars.");
		let p2 = adjust_money (List.nth st.players 1) (0-st.b_blind) in
		let p2' = {p2 with pot_money = st.b_blind} in
		print_endline (p2.id ^ " has posted a big blind of " ^ (string_of_int
		st.b_blind) ^ " dollars.");
		{st with deal_number = 1; players = p1'::[p2']; curr_bet = st.b_blind;
		pot = st.b_blind + st.s_blind} in

	try
		if st.deal_number = 0 then let st' = post_blinds st in game_loop st'
		else if st.deal_number = 1 then bet1 st 0 0
		else if st.deal_number = 2 then deal_flop st
		else if st.deal_number = 4 then deal_turn st
		else if st.deal_number = 6 then deal_river st
		else bet st 0 0
	with
	| QuitGame -> print_endline "\nGame over :(\n";

(* calls do' to receive the new state for a valid command, or reprompts the
 * user if they enter an invalid command given the state *)
and do_command st player com valid_cmds =
	try
		let c = get_command com in
		if c = Quit then raise QuitGame
		else if c = Help then
			(print_endline instructions;
			print_state st;
			let cards = List.map card_to_string player.hole_hand in
				let () = match cards with
				| a::b::[] -> print_endline ("Your cards: " ^ a ^ " | " ^ b);
				| _ -> failwith "Unreachable" in
			print_endline ("Your funds: $" ^ string_of_int player.money);
			print_endline "\nWhat would you like to do?";
			print_string "> ";
			do_command st player (read_line ())) valid_cmds
		else
			match c with
			| Raise n ->
				if List.mem (Raise 0) valid_cmds then do' c player st
				else raise Illegal
			| Bet n ->
				if List.mem (Bet 0) valid_cmds then do' c player st
				else raise Illegal
			| _ -> if List.mem c valid_cmds then do' c player st
				else raise Illegal

	with | Illegal -> (print_endline "\nPlease enter a valid command.";
		print_endline "What would you like to do?"; print_string "> ";
		do_command st player (read_line ()) valid_cmds)

(* returns true if there is a single player that is active in the current
 * round, false if both are still active *)
and single_player st =
		let p1 = List.nth st.players 0 in
		let p2 = List.nth st.players 1 in
		if p1.is_active && p2.is_active then false else true

(* handles the situation where one player goes "all in", and ends the round
 * differently based on if the other player calls or folds. *)
and all_in st index r =
	let cards' = if List.length st.comm_cards = 0 then
	flop st.deck |> turn st.deck |> river st.deck
	else if List.length st.comm_cards = 3 then
	turn st.deck st.comm_cards |> river st.deck
	else if List.length st.comm_cards = 4 then river st.deck st.comm_cards
	else st.comm_cards in
	let st' = {st with comm_cards = cards'} in
	let player = List.nth st'.players index in
	let p2 = if index = 0 then List.nth st'.players 1
	else List.nth st'.players 0 in
	ANSITerminal.erase Screen;
	print_endline (player.id ^ " has gone all in.");
	print_endline ("New bet: " ^ string_of_int st'.curr_bet);
	let cards = match List.map card_to_string st.comm_cards with
	| [] -> "none"
	| a::b::c::d::e::[] -> a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ", " ^ e
	| a::b::c::d::[] -> a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d
	| a::b::c::[] -> a ^ ", " ^ b ^ ", " ^ c
	| _ -> failwith "Unreachable" in
	print_endline ("Community cards: " ^ cards);
	let cards' = List.map card_to_string player.hole_hand in
		let () = match cards' with
		| a::b::[] -> print_endline ("Your cards: " ^ a ^ ", " ^ b);
		| _ -> failwith "Unreachable" in
	print_endline ("The next player may fold, or call to begin the showdown.\n");
	if player.is_AI then
		(print_endline "Would you like to fold or call the bet?";
		print_string "> ";
		let com = read_line () in
		let state = do_command st' p2 com [Call; Fold] in
		end_game state)
	else
		let num = int_of_float (to_call r st' p2) in
		ANSITerminal.erase Screen;
		let com = if num = 0 then (print_endline "AI has called the bet."; "fold")
		else "call" in
		let state = do_command st' p2 com [Call; Fold] in
		end_game state

(* handles the first betting round *)
and bet1 st index r =

	if single_player st then end_game st
	else
	let bb = List.nth st.players 1 in
	let sb = List.nth st.players 0 in
	let player = List.nth st.players index in
	if ((bb.pot_money = sb.pot_money) && (bb.last_cmd <> None))
	|| player.money = 0 then
		(let players' = [{sb with pot_money = 0; has_raised = false; last_cmd
			= None};
		{bb with pot_money = 0; has_raised = false; last_cmd = None}] in
		let st' = {st with deal_number = st.deal_number + 1; players = players';
		curr_bet = 0} in game_loop st')

	else
		if (index = 0) then (*small blind*)
			(if (not player.is_AI) then
				(print_state st;
				print_endline "YOUR TURN.\n";
				let cards = List.map card_to_string player.hole_hand in
				let () = match cards with
				| a::b::[] -> print_endline ("Your cards: " ^ a ^ ", " ^ b);
				| _ -> failwith "Unreachable" in
				print_endline ("Your funds: $" ^ string_of_int player.money);

				if (bb.last_cmd = None) then
					(print_endline "You may fold, call, or raise the big blind\n";
					print_endline "What would you like to do?";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					if r' = 4900 then all_in st' index r' else
					bet1 st' ((index + 1) mod 2) r')
				else
					(print_endline "You may fold or call the current bet \n";
					print_endline "What would you like to do?";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call] in
					let r' = st'.curr_bet - st.curr_bet in
					bet1 st' ((index + 1) mod 2) r'))

			else
				(if bb.last_cmd = None then
					(let num = int_of_float (bet_amount st player) in
					let com =
						if num = 0 then
							(print_endline "\nAI HAS CALLED THE BIG BLIND."; "call")
						else
						(print_endline ("\nAI HAS RAISED $"^(string_of_int num));
						 "Raise " ^ (string_of_int num)) in
					let st' = do_command st player com [Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					if r' = 4900 then all_in st' index r' else
					bet1 st' ((index + 1) mod 2) r')
				else
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
							print_endline "\n AI HAS FOLDED."; "fold")
						else
						(ANSITerminal.erase Screen;
							print_endline "\n AI HAS CALLED THE CURRENT BET";
						"call") in
					let st' = do_command st player com [Fold; Call] in
					let r' = st'.curr_bet - st.curr_bet in
					bet1 st' ((index + 1) mod 2) r')))

		else (*big blind*)
			(if (not player.is_AI) then
				(print_state st;
				print_endline "YOUR TURN.\n";
				let cards = List.map card_to_string player.hole_hand in
				let () = match cards with
				| a::b::[] -> print_endline ("Your cards: " ^ a ^ ", " ^ b);
				| _ -> failwith "Unreachable" in
				print_endline ("Your funds: $" ^ string_of_int player.money);

				if sb.last_cmd = Some "call" then
					(print_endline "You may check or raise the current bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Check; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet1 st' ((index + 1) mod 2) r')
				else
					(print_endline "You may fold, call, or raise the current bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet1 st' ((index + 1) mod 2) r'))

			else
				(if sb.last_cmd = Some "call" then
					(let num = int_of_float (bet_amount st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
							print_endline "\nAI HAS CHECKED"; "check")
						else
						(ANSITerminal.erase Screen;
							print_endline ("\nAI HAS RAISED $"^(string_of_int num));
						 "bet " ^ string_of_int num) in
					let st' = do_command st player com [Check; Bet 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet1 st' ((index + 1) mod 2) r')
				else
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
							print_endline "\nAI HAS FOLDED"; "fold")
						else if num = r then
						(ANSITerminal.erase Screen;
							print_endline "\nAI HAS CALLED"; "call")
						else
						(ANSITerminal.erase Screen;
							print_endline ("\nAI HAS RAISED $"^(string_of_int (num-r)));
						  "raise " ^ (string_of_int (num-r))) in
					let st' = do_command st player com [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet1 st' ((index + 1) mod 2) r')))

(* handles the second through fourth betting rounds *)
and bet st index r =

	if single_player st then end_game st
	else
	let p1 = List.nth st.players 0 in
	let p2 = List.nth st.players 1 in
	let player = List.nth st.players index in

	if p1.last_cmd = Some "call" || p2.last_cmd = Some "call" ||
	p2.last_cmd = Some "check" then
		(if st.deal_number = 7 then end_game st else
		let players' = [{p1 with pot_money = 0; has_raised = false; last_cmd
		= None};
		{p2 with pot_money = 0; has_raised = false; last_cmd = None}] in
		let st' = {st with deal_number = st.deal_number + 1; players = players';
		curr_bet = 0} in
		game_loop st')

	else
		if player = p1 then
			(if not player.is_AI then
				(print_state st;
				print_endline "YOUR TURN.\n";
				let cards = List.map card_to_string player.hole_hand in
				let () = match cards with
				| a::b::[] -> print_endline ("Your cards: " ^ a ^ ", " ^ b);
				| _ -> failwith "Unreachable" in
				print_endline ("Your funds: $" ^ string_of_int player.money);

				if p2.last_cmd = None then
					(print_endline "You may check or bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Check; Bet 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else if p2.last_cmd = Some "bet" then
					(print_endline "You may fold, call, or raise the current bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else
					(print_endline "You may fold or call.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call] in
					let r' = st'.curr_bet - st.curr_bet in
					bet st' ((index + 1) mod 2) r'))
			else
				(if p2.last_cmd = None then
					(let num = int_of_float (bet_amount st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CHECKED"; "check")
						else
						(ANSITerminal.erase Screen;
						 print_endline ("\nAI HAS BET $"^(string_of_int num));
						  "bet " ^ string_of_int num) in
					let st' = do_command st player com [Check; Bet 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else if p2.last_cmd = Some "bet" then
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS FOLDED"; "fold")
						else if num = r then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CALLED"; "call")
						else
						(ANSITerminal.erase Screen;
						 print_endline ("\nAI HAS RAISED $"^(string_of_int (num-r)));
						  "raise " ^ (string_of_int (num-r))) in
					let st' = do_command st player com [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS FOLDED"; "fold")
						else
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CALLED"; "call") in
					let st' = do_command st player com [Fold; Call] in
					let r' = st'.curr_bet - st.curr_bet in
					bet st' ((index + 1) mod 2) r')))
		else
			(if not player.is_AI then
				(print_state st;
				print_endline "YOUR TURN.\n";
				let cards = List.map card_to_string player.hole_hand in
				let () = match cards with
				| a::b::[] -> print_endline ("Your cards: " ^ a ^ ", " ^ b);
				| _ -> failwith "Unreachable" in
				print_endline ("Your funds: $" ^ string_of_int player.money);

				if p1.last_cmd = Some "check" then
					(print_endline "You may check or bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Check; Bet 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else if p1.last_cmd = Some "bet" then
					(print_endline "You may fold, call, or raise the current bet.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else
					(print_endline "You may call or fold.";
					print_endline "What would you like to do? ";
					print_string "> ";
					let line = read_line () in
					let st' = do_command st player line [Call; Fold] in
					let r' = st'.curr_bet - st.curr_bet in
					bet st' ((index + 1) mod 2) r'))
			else
				(if p1.last_cmd = Some "check" then
					(let num = int_of_float (bet_amount st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CHECKED"; "check")
						else
						(ANSITerminal.erase Screen;
						 print_endline ("\nAI HAS BET $" ^ (string_of_int num));
 							"bet " ^ string_of_int num) in
					let st' = do_command st player com [Check; Bet 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else if p1.last_cmd = Some "bet" then
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS FOLDED"; "fold")
						else if num = r then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CALLED"; "call")
						else
						(ANSITerminal.erase Screen;
						 print_endline ("\nAI HAS RAISED $"^(string_of_int (num-r)));
							"raise " ^ (string_of_int (num-r))) in
					let st' = do_command st player com [Fold; Call; Raise 0] in
					let r' = st'.curr_bet - st.curr_bet in
					let player' = List.nth st'.players index in
					if player'.money = 0 && player.money <> 0 then
					all_in st' index r'
					else bet st' ((index + 1) mod 2) r')
				else
					(let num = int_of_float (to_call r st player) in
					let com =
						if num = 0 then
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS FOLDED"; "fold")
						else
						(ANSITerminal.erase Screen;
						 print_endline "\nAI HAS CALLED"; "call") in
					let st' = do_command st player com [Fold; Call] in
					let r' = st'.curr_bet - st.curr_bet in
					bet st' ((index + 1) mod 2) r')))

(* handles the ending of a round, either in a showdown or if one player folds.*)
and end_game st =
	let get_player name st =
		match st.players with
		| p1::p2::[] -> if name = p1.id then p1 else p2
		| _ -> failwith "Unreachable" in

	try
		let p1 = List.nth st.players 0 in
		let p2 = List.nth st.players 1 in
		let w = if p1.is_active && p2.is_active then
		get_player (winner (make_player_cards st)) st
		else if p1.is_active then p1 else p2 in
		let cards = List.map card_to_string w.hole_hand in
		let () = match cards with
				| a::b::[] -> ( print_endline ("<><><><><><><><><><><><><><><><><><>"^
																			"<><><><><><><><><><><><><><><><><><><>");
												print_endline ("Winner's cards: " ^ a ^ " | " ^ b);)
				| _ -> failwith "Unreachable" in
		let cards' = match List.map card_to_string st.comm_cards with
		| [] -> "none"
		| a::b::c::d::e::[] -> a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ", " ^ e
		| a::b::c::d::[] -> a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d
		| a::b::c::[] -> a ^ ", " ^ b ^ ", " ^ c
		| _ -> failwith "Unreachable" in
		print_endline ("Community cards: " ^ cards');
		print_endline (w.id ^ " has won the round. Their final earnings: $" ^
		string_of_int (w.money + st.pot));
		let deck = make_deck () |> shuffle in
		let p1'gain = if w = p2 then st.pot else 0 in
		let p2'gain = if w = p1 then st.pot else 0 in
		let p1' = {p2 with hole_hand = deal_hole deck; money = p2.money + p1'gain;
		pot_money = 0; is_active = true; has_raised = false; last_cmd = None} in
		let p2' = {p1 with hole_hand = deal_hole deck; money = p1.money + p2'gain;
		pot_money = 0; is_active = true; has_raised = false; last_cmd = None} in

		if p1'.money = 0 || p1'.money < st.s_blind then
			(print_endline (p1'.id ^ " is out of money.");
			print_endline (p2'.id ^ " wins!"); raise QuitGame)
			else if p2'.money = 0 || p2'.money < st.b_blind then
			(print_endline (p2'.id ^ " is out of money.");
			print_endline (p1'.id ^ " wins!"); raise QuitGame)
		else
		(print_endline ("<><><><><><><><><><><><><><><><><><>"^
									"<><><><><><><><><><><><><><><><><><><>");
		print_endline "\n\nA new round will commence.\n");
		let st' = {round_number = st.round_number + 1; deal_number = 0; deck =
		deck;
		b_blind = st.b_blind; s_blind = st.s_blind; players = [p1';p2'];
		curr_bet = 0; pot = 0; comm_cards = []} in
		game_loop st'

	with | Tie -> (let p1 = List.nth st.players 0 in
		let p2 = List.nth st.players 1 in
		let human = if p1.is_AI then p1 else p2 in
		print_endline ("You and AI have tied. Your Final earnings: $" ^
		string_of_int (human.money + (st.pot/2)));

		let deck = make_deck () |> shuffle in
		let p1' = {p2 with hole_hand = deal_hole deck; money = p2.money +
		(st.pot/2); pot_money = 0; is_active = true; has_raised = false;
		last_cmd = None} in
		let p2' = {p1 with hole_hand = deal_hole deck; money = p1.money +
		(st.pot/2); pot_money = 0; is_active = true; has_raised = false;
		last_cmd = None} in

	if p1'.money = 0 || p1'.money < st.b_blind then
			(print_endline (p1'.id ^ " is out of money.");
			print_endline (p2'.id ^ " wins!"); raise QuitGame)
			else if p2'.money = 0 || p2'.money < st.b_blind then
			(print_endline (p2'.id ^ " is out of money.");
			print_endline (p1'.id ^ " wins!"); raise QuitGame)
		else
		print_endline "\n\nA new round will commence.\n";
		let st' = {round_number = st.round_number + 1; deal_number = 0; deck =
		deck;
		b_blind = st.b_blind; s_blind = st.s_blind; players = [p1';p2'];
		curr_bet = 0; pot = 0; comm_cards = []} in
		game_loop st')


(* deals the flop *)
and deal_flop st =
	print_endline "\nThe flop has been dealt.";
	let f = flop st.deck in
	let st' = {st with deal_number = st.deal_number + 1; curr_bet = 0;
	comm_cards = f} in
	game_loop st'

(* deals the turn *)
and deal_turn st =
	print_endline "\nThe turn has been dealt.";
	let t = turn st.deck st.comm_cards in
	let st' = {st with deal_number = st.deal_number + 1; curr_bet = 0;
	comm_cards = t} in
	game_loop st'

(* deals the river *)
and deal_river st =
	print_endline "\nThe river has been dealt.";
	let r = river st.deck st.comm_cards in
	let st' = {st with deal_number = st.deal_number + 1; curr_bet = 0;
	comm_cards = r} in
	game_loop st'

(* entry point to the game from main.ml. [main] creates the list of players
 * based on the user's input and creates a new state to begin the game on. *)
let main () =
	ANSITerminal.resize 74 40;
	let name = read_line () in
	let deck = make_deck () |> shuffle in
	let p1 = make_player name false deck in
	let p2 = make_player "AI" true deck in
	let st = new_state [p1;p2] deck in
	game_loop st