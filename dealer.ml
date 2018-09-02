open Card

(* Mutable data structure that holds a unique copy of each card in a deck,
must be mutable such that dealt cards are removed from the table*)
type deck = {mutable crnt_card : int; mutable cards : card array}

(* Cards dealt to each player in the round, must have length = 2 *)
type hole_hand = card list

(* Community Cards that are available to all players*)
type comm_cards = card list

(*[get_suit n] is a helper funciton that alows the make_deck to map an integer
from 1 - 4 to a card rank such that we can easily make a deck of cards using
a for loop. n must be between 1 and 4 inclusive*)
let get_suit n =
	if n = 1 then Diamonds
	else if n = 2 then Hearts
	else if n = 3 then Clubs
	else Spades

(* [rep_ok d] checks to see if the representation invariant of the deck is
satisfied and returns the deck if it is, if not, it raises failure *)
let rep_ok d =
	if Array.length (d.cards) <> 52 then failwith "not a valid deck of cards"
	else
		let list_cards = Array.to_list(d.cards) in
		let rec check_dupls l accu=
		match l with
		|[] -> accu
		|h::t -> check_dupls t (accu && List.mem h t) in
	if check_dupls list_cards true then d
	else failwith "deck contains duplicates"

(*Makes a new deck of 52 cards*)
let make_deck () =
	let cards = Array.make 52 (12, Spades) in
	let i = ref 0 in
	for curr_suit = 1 to 4 do
		for rnk = 0 to 12 do
			Array.set cards !i (rnk, get_suit curr_suit);
			i := !i+1
		done
	done;
	{crnt_card = 0; cards = cards}

(* Shuffles a deck so as to, size of deck must remain the same before and after
the shuffle. Returns a deck with different ordering of cards*)
let shuffle deck =
	let () = Random.self_init() in
	let n = ref 1000 in
	for i = 1 to !n do
		let p1 = Random.int 52 in
		let p2 = Random.int 52 in
		let temp = Array.get deck.cards p1 in
		Array.set deck.cards p1 (Array.get deck.cards p2);
		Array.set deck.cards p2 temp
	done;
	deck


(* Return a hole hand from the deck of cards, update the deck such that the
cards that have been dealt are removed from the deck and cannot be recieved
by other players if the same deck is being used to deal to another player*)
let deal_hole deck =
	let hole = [(Array.get deck.cards deck.crnt_card);
				(Array.get deck.cards (deck.crnt_card+1))] in
	deck.crnt_card <- deck.crnt_card+2;
	hole

(* Burn a card from the deck and return a flop of cards from the deck (first
three cards shown to all players, remove the cards in the flop from the deck*)
let flop deck =
	let flp = [(Array.get deck.cards (deck.crnt_card+1));
				(Array.get deck.cards (deck.crnt_card+2));
				(Array.get deck.cards (deck.crnt_card+3))] in
	deck.crnt_card <- deck.crnt_card + 4;
	flp

(* Remove a card from the deck and append a card from the deck to the list of
  community cards, remove the card added to the list of community cards from
  the deck*)
let turn deck comm_cards =
	let next_card = Array.get deck.cards (deck.crnt_card+1) in
	deck.crnt_card <- deck.crnt_card + 2;
	next_card::comm_cards

(* Remove a card from the deck and append a card from the deck to the list of
  community cards, remove the card added to the list of community cards from
  the deck*)
let river deck comm_cards =
	let next_card = Array.get deck.cards (deck.crnt_card+1) in
	deck.crnt_card <- deck.crnt_card + 2;
	next_card::comm_cards














