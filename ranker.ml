open Card
open Dealer
open State

(* List of tuples mapping each player to the cards that they can use to make a
hand*)
type player_cards = (string * card list) list

(* Tie is raised when there is a draw in the ranker, i.e the best hand that
either player can make is the same*)
exception Tie

(* Illegal is raised when the user tries to pass an invalid data type to a
function *)
exception Illegal

(* A hand_ranking system meant to simulate a 6-bit number
 * The left most field/bit is a number from 0 to 8
 * representing 1 of the 9 types of poker hands.
 * Each succecding field represents a number
 * identifying a property of the hand..
 * these fields are in order of importance.
 * e.g 2C 2H KH QH AS is {1,0,12,11,10,0}
 * e.g 4C 5H 6S 7H 8S is {4,6,0,0,0,0} since only
 * high card matters for straights
 *)
type hand_rank = {typ : int; b0 : rank; b1 : rank; b2 : rank; b3 : rank; b4 : rank}

(* [compare_card c1 c2] is a compare method for card types
 * that is postiive if c1 has a higher rank than c2
 * negative if c1 has a lower rank than c2
 * and zero otherwise
 *)
let compare_card c1 c2 =
	if (fst c1) > (fst c2) then 1
	else if (fst c1) < (fst c2) then -1
	else 0

(* [rep_ok pl_crds] checks to see if a player_cards structure satisfies it's
representation invariant and returns the structure if it does. If it doesn't then
the function raises failure *)
let rep_ok pl_crds =
	let rec get_cards plc accu=
	match plc with
	|[] -> accu
	|h::t -> get_cards t ((snd h)@accu) in
	let all_cards = get_cards pl_crds [] in
		let rec check_dupls l accu=
			match l with
			|[] -> accu
			|h::t -> check_dupls t (accu && List.mem h t) in
		if check_dupls all_cards true then pl_crds
		else failwith "Duplicates in player cards"

(* [get_rank_list hand] takes a list of cards and
 * returns a list of their ranks in ascending order
 *)
let get_rank_list hand =
	let ranks = List.map (fun x -> fst x) hand in
	List.sort compare ranks

(* [get_hc hand] takes a list of cards and
 * returns a hand_rank that denotes a high-card
 * hand and their relevent bits
 *)
let get_hc hand =
	let hand_ranks = get_rank_list hand in
	match hand_ranks with
	| r1::r2::r3::r4::r5::[] -> {typ = 0; b0 = r5; b1 = r4; b2 = r3; b3 = r2; b4 = r1}
	| _ -> failwith "Unreachable"

(* [has_twin c lst] takes a card c and a list of
 * cards lst and checks if lst contains
 * a card with the same rank as c
 *)
let rec has_twin c lst =
	match lst with
	| [] -> false
	| h::t -> if (fst c) = (fst h) then true else has_twin c t

let is_trip hand =
	let hand' = get_rank_list hand in
	let rec is_trip' rnks =
		match rnks with
		| a::b::c::t ->
			if (a = b) && (a = c) then true
			else is_trip' (b::c::t)
		| _ -> false
	in is_trip' hand'

let is_quad hand =
	let hand' = get_rank_list hand in
	let rec is_quad' rnks =
		match rnks with
		| a::b::c::d::t ->
			if (a = b) && (a = c) && (a = d) then true
			else is_quad' (b::c::d::t)
		| _ -> false
	in is_quad' hand'

(* [get_trip hand] takes a list of 5 cards hand
 * and returns a hand_rank option denoting
 * a triple hand with relevent bits.
 * get_trip will only return some hand_rank
 * if get_trip is uniquely a triple or full-house
 * other wise None
 *)
let rec get_trip hand =
	let hand' = List.sort compare_card hand in
	match hand' with
	| c1::c2::c3::c4::c5::t ->
		if is_quad hand then None
		else if (fst c1)=(fst c2) && (fst c1)=(fst c3) then
			Some {typ = 3; b0 = fst c1; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
		else if (fst c2)=(fst c3) && (fst c2)=(fst c4) then
			Some {typ = 3; b0 = fst c2; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
		else if (fst c3)=(fst c4) && (fst c3)=(fst c5) then
			Some {typ = 3; b0 = fst c3; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
		else
			None
	| _ -> None

(* [get_sraight hand] returns a hand_rank option
 * that outputs Some hand_rank if the hand is a straight
 * and None otherwise. get_straight also accounts for Ace
 * as a high or low card evaluates the Ace value appropriately
 *)
let get_straight hand =
	let hand' = get_rank_list hand in
	if hand' = [0;1;2;3;12] then
		Some {typ = 4; b0 = 3; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
	else
		match hand' with
		| r1::r2::r3::r4::r5::[] ->
			if (r1 = r2-1) && (r2 = r3-1) && (r3 = r4-1) && (r4 = r5-1) then
				Some {typ = 4; b0 = r5; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
			else None
		| _ -> failwith "Unreachable"

(* [get_flush hand] returns a hand_rank option that
 * outputs Some hand_rank if the hand is a flush
 * and None otherwise
 *)
let get_flush hand =
	let suits = List.map (fun x -> snd x) hand in
	match suits with
	| s1::s2::s3::s4::s5::[] ->
		if (s1 = s2) && (s2 = s3) && (s3 = s4) && (s4 = s5) then
			let hr = get_hc hand in
			Some {typ = 5;
				  b0 = hr.b0;
				  b1 = hr.b1;
				  b2 = hr.b2;
				  b3 = hr.b3;
				  b4 = hr.b4}
		else None
	| _ -> failwith "Unreachable"

(* [get_striaght_ flush hand] returns a hand_rank option
 * that outputs Some hand_rank if the hand is both a
 * straight and a flush and None otherwise
 *)
let get_straight_flush hand =
	let sr_hv = get_straight hand in
	let fl_hv = get_flush hand in
	match sr_hv, fl_hv with
	| Some a, Some b -> Some {typ = 8; b0 = a.b0; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
	| _ -> None

(* [get_full_house hand] returns a hand_rank option that
 * outputs Some hand_rank if the hand is a full-house
 * and None otherwise
 *)
let get_full_house hand =
	let trip = get_trip hand in
	match trip with
	| None -> None
	| Some hr ->
		let others = List.filter (fun x -> (fst x) <> hr.b0) hand in
		(match others with
		| h::t ->
			if (has_twin h t) then
			 	Some {typ = 6; b0 = hr.b0; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
			else None
		| _ -> failwith "Unreachable")

(* [get_pair_val rnk hand] is a helper method
 * for get_pair that takes a 3-tuple rnk.
 * rnk desginates 1 of three hands
 * pair, two-pair, or quad. Which is represented
 * by the first entry. Therefore, the
 * first entry must be 1,2,or 7 else
 * an error is thrown.
 * furthermore, hand must be one the aformentioned
 * hands.
 * get_pair_val returns a hand_rank
 * that designates the hand as either one
 * of the three possible hands
 *)
let get_pair_val rnk hand =
	let hand_ranks = get_rank_list hand in
	match rnk with
	| (1, a, _) ->
		let others = List.filter (fun x -> x <> a) hand_ranks in
		(match others with
		| r1::r2::r3::[] -> {typ = 1; b0 = a; b1 = r3; b2 = r2; b3 = r1; b4 = 0}
		| _ -> failwith "Unreachable")
	| (2, a, b) ->
		let others = List.filter (fun x -> ((x <> a) && (x <> b))) hand_ranks in
		(match others with
		| r1::[] -> {typ = 2; b0 = a; b1 = b; b2 = r1; b3 = 0; b4 = 0}
		| _ -> failwith "Unreachable")
	| (7, a, _) -> {typ = 7; b0 = a; b1 = 0; b2 = 0; b3 = 0; b4 = 0}
	| _ -> failwith "Unreachable"

(* [get_pair hand] returns a hand_rank option that
 * outputs Some hand_rank if the hand is a pair,
 * two-pair, or four-of-a-kind. The hand_rank
 * appropriately distinguishes the hand
 * and provides the neccesary bits
 * returns None otherwise
 * Precondition: hand cannot be a triple
 *)
let get_pair hand =
	let hand' = List.sort compare_card hand in
	let rec get_pair' hnd best_hnd =
		match hnd with
		| [] -> best_hnd
		| h::[] -> best_hnd
		| h::s::t ->
			if has_twin h (s::t) then
			(match best_hnd with
			| None -> get_pair' t (Some (1, fst h, -1))
			| Some (_, a,b) ->
				if (fst h) <  a then Some (2, a, fst h)
				else if (fst h) > a then Some (2, fst h, a)
				else Some (7, a, b))
			else get_pair' (s::t) best_hnd
	in
	let rnk = get_pair' hand' None in
	match rnk with
	| None -> None
	| Some r -> Some (get_pair_val r hand)

(* [get_hand_rank hand] returns the hand_rank of a given
 * list of 5 cards. Use this to get the type of hand
 * a player has and its value.
 * Precondition: hand must be a valid 5 card
 * poker hand.
 *)
let get_hand_rank hand =
	let hr =
		if (get_straight_flush hand) <> None then
			get_straight_flush hand

		else if (get_full_house hand) <> None then
			get_full_house hand

		else if (get_flush hand) <> None then
			get_flush hand

		else if (get_straight hand) <> None then
			get_straight hand

		else if (get_trip hand <> None) then
			(* let () = print_endline "Here" in *)
			get_trip hand

		else if (get_pair hand <> None)	then
			get_pair hand

		else Some (get_hc hand)
	in
	match hr with
	| Some a -> a
	| None -> failwith "Unreachable"

(* [is_winning_hand hr1 hr2] is true of hr1
 * is greater than hr2, false if hr1 < hr2
 * and raises a tie exception otherwise
 * Use this to determine if player1's hands beats player2s
 *)
let is_winning_hand hr1 hr2 =
	if hr1.typ = hr2.typ then
		if hr1.b0 = hr2.b0 then
			if hr1.b1 = hr2.b1 then
				if hr1.b2 = hr2.b2 then
					if hr1.b3 = hr2.b3 then
						if hr1.b4 = hr2.b4 then
							raise Tie
						else hr1.b4 > hr2.b4
					else hr1.b3 > hr2.b3
				else hr1.b2 > hr2.b2
			else hr1.b1 > hr2.b1
		else hr1.b0 > hr2.b0
	else hr1.typ > hr2.typ

(* [comp_hands hr1 hr2] is a compare function for hand ranks.
 * comp_hands is postive if hr1 > hr2, negative if hr1 < hr2 and
 * 0 otherwise
 * Can be used in List.sort to sort a list of hand ranks
 * Use this to determine the winner in a game with > 2 players
 *)
let comp_hands hr1 hr2 =
	try
		if is_winning_hand hr1 hr2 then 1
		else -1
	with
	| Tie -> 0

let rec comb arr len start result_arr lst_of_res =
	if len = 0 then
		lst_of_res := ((Array.to_list result_arr)::(!lst_of_res))
	else
		for i = start to ((Array.length arr)-len) do
			Array.set result_arr ((Array.length result_arr)-len) (Array.get arr i);
			comb arr (len-1) (i+1) result_arr lst_of_res
		done

let rec get_last_elt lst =
	match lst with
	| [] -> failwith "Unreachable"
	| a::[] -> a
	| h::t -> get_last_elt t

let get_player_hand plyr_crds =
	match plyr_crds with
	| c1::c2::c3::c4::c5::c6::c7::[] ->
		let all_cards = c1::c2::c3::c4::c5::c6::c7::[] in
		let all_cards_arr = Array.of_list all_cards in
		let lst_of_res = ref [] in
		comb all_cards_arr 5 0 (Array.make 5 (0, Spades)) lst_of_res;
		let all_hands = List.map (fun x -> get_hand_rank x) !lst_of_res in
		let all_hands' = List.sort (comp_hands) all_hands in
		get_last_elt all_hands'
	| _ -> failwith "Unreachable"


(* Takes in the current state of the game and returns the player_cards that each
player can use to make a hand *)
let make_player_cards (stat:state): player_cards =
	let p_list = stat.players in
	let comm_cards = stat.comm_cards in
	let player_cards = List.map (fun x -> (x.id,x.hole_hand)) p_list in
	List.map (fun (x,y) ->(x,y@comm_cards)) player_cards

(*Precondtion: lst is sorted in ascending order*)
let rec has_highest compare lst =
	match lst with
	| [] -> raise Illegal
	| a::[] -> true
	| a::b::[] ->
		if compare a b = 0 then false
		else true
	| h::t -> has_highest compare t


(* Returns the winner of the round if every player must show their hands
(all bets are place, river has gone down) based on who has highest ranked hand
as per the rules of holdem*)
let winner (plyr_crds:player_cards) : string =
	let hands=List.map (fun (x,y) -> (x,get_player_hand y)) plyr_crds in
	let ranked_hands = List.sort (fun (x1,y1) (x2,y2)-> comp_hands y1 y2) hands in

	if has_highest comp_hands (List.map (fun (x,y) -> y) ranked_hands) then
		fst (get_last_elt ranked_hands)
	else raise Tie


















