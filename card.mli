
(* Describes the type of suit a card is*)
type suit = Hearts | Diamonds | Clubs | Spades

(* Describes rank of a card, 0-12 correspond to themselves, 9 - 12 correspond
to Jack, Queen, King, Ace respectively. rank must be <=12 and >=0 *)
type rank = int

(* Defines a card as a combination of it's rank and it's suit. eg. (11,Diamonds)
corresponds to the Jack of Diamonds

AF: card = rank * suit maps a rank between 0-12 to a card rank from 2-10 and the
face cards of a deck: Jack, Queen, King and Ace with their respective suit

RI: Rank must be between 0 and 12 and suit must be a valid suit constructor*)
type card = rank * suit

(* [card_to_string] returns the string representation of a card with the rank
followed by the suit, requires that the card be a valid rank (0-12). e.g.
card_to_string (11,Diamonds) would return "K♦"*)
val card_to_string : card -> string

