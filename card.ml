(* Describes the type of suits a card can have*)
type suit = Hearts | Diamonds | Clubs | Spades

(* Describes rank of a card, 0-8 correspond to 2-10, 9 - 12 correspond
to Jack, Queen, King, Ace respectively. rank must by <=12 *)
type rank = int

(* Defines a card as a combination of it's rank and it's suit. eg. (11,Diamonds)
corresponds to the Jack of Diamonds

AF: card = rank * suit maps a rank between 0-12 to a card rank from 2-10 and the
face cards of a deck: Jack, Queen, King and Ace with their respective suit

RI: Rank must be between 0 and 12 and suit must be a valid suit constructor*)
type card = rank * suit

(* [rep_ok card] checks if the card data type satisfies it's representation
invariant *)
let rep_ok card =
  if fst card >12 || fst card < 0 then
    failwith "Invalid Card"
  else card

(* [string_of_ranker r] is a helper for the card_to_string function that
mapys the rank of a card to it's string representation *)
let string_of_rank r : string =
  match r with
  |x when x<9 && x>=0-> string_of_int (x+2)
  |9 -> "J"
  |10 -> "Q"
  |11 -> "K"
  |12 -> "A"
  | _ -> failwith "Invalid Rank"


(*[string_of_suit s] returns a string representation of an input suit*)
let string_of_suit (s:suit) :string =
  match s with
  |Hearts -> "♥"
  |Spades -> "♠"
  |Clubs -> "♣"
  |Diamonds -> "♦"
  |_ -> failwith "Invalid Suit"


(* [card_to_string] returns the string representation of a card with the rank
followed by the suit, requires that the card be a valid rank (0-12). e.g.
card_to_string (11,Diamonds) would return "K♦"*)
let card_to_string card =
  match card with
  |(r,s) -> string_of_rank r ^ "" ^string_of_suit s
  |_ -> failwith "Invalid Card"