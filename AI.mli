open Card
open Dealer
open Ranker
open State

(* returns the probability of winning the round based on the AI's current hand*)
val probability_win: state -> player -> float

(* Returns 0 if the AI should fold, a value equal to the human player's raise
 * if the AI should call, or a value larger than that raise if the AI should
 * raise. *)
val to_call: int -> state -> player -> float

(* returns the amount of money the AI ought to bet for this current hand such
that the expectation of return is positive. A value of zero means no additional
bet *)
val bet_amount: state -> player-> float