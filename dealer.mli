open Card

(* Mutable data structure that holds a unique copy of each card in a deck,
must be mutable such that dealt cards are removed from the table.

Abstraction Function: Each entry of the deck's card array is a card in the deck,
the 52 entries in the deck array are the 52 cards in a deck. Each entry is of
the type card. the curr_card represents where in the deck the deal is, rather
than ejecting cards and placing them at the end of the deck we move through
the indicies as cards are dealt, simulating how you move down a stack of cards
as you deal them

Representation Invariant (RI):the deck must always have 52 indicies as it has to
always hold 52 cards, cards should never be removed or deleted. Moreover the
deck cannot contain duplicates as a deck must have only unique cards*)
type deck = {mutable crnt_card : int; mutable cards : card array}

(* Cards dealt to each player in the round, must have length = 2 *)
type hole_hand = card list

(* Community Cards that are available to all players*)
type comm_cards = card list

(*Makes a new deck of 52 cards*)
val make_deck: unit -> deck

(* Shuffles a deck so as to, size of deck must remain the same before and after
the shuffle. Returns a deck with different ordering of cards*)
val shuffle: deck -> deck

(* Return a hole hand from the deck of cards, update the deck such that the
cards that have been dealt are removed from the deck and cannot be recieved
by other players if the same deck is being used to deal to another player*)
val deal_hole: deck -> hole_hand

(* Burn a card from the deck and return a flop of cards from the deck (first
three cards shown to all players, remove the cards in the flop from the deck*)
val flop: deck -> comm_cards

(* Remove a card from the deck and append a card from the deck to the list of
  community cards, remove the card added to the list of community cards from
  the deck*)
val turn: deck -> comm_cards -> comm_cards

(* Remove a card from the deck and append a card from the deck to the list of
  community cards, remove the card added to the list of community cards from
  the deck*)
val river: deck -> comm_cards -> comm_cards
