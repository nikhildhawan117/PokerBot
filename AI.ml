open Card
open Dealer
open Ranker
open State

exception Invalid

(* [num_active_players st] returns the number of active playes in the round
in order to generate the number of opponents the AI will face in the simulation
(for testing purposes, now outdated) *)
let rec num_active_players st =
    let rec accu_actives players =
    match players with
    |[] -> 0
    |h::t -> if h.is_active then 1 + accu_actives t else accu_actives t
  in accu_actives st.players

(* [remove_cards cards deck] removes the cards the AI has from the deck that
will be used in the simulation so as to ensure no simulated player is dealt
the cards the AI has *)
let rec remove_cards cards deck =
 let curr_cards = deck.cards |> Array.to_list in
   let rec update_deck deck cards =
   match deck with
   |[] -> []
   |h::t -> if List.mem h cards then
            update_deck t cards
            else h :: update_deck t cards
  in let new_deck = update_deck curr_cards cards in
  deck.cards <- Array.of_list new_deck; deck

(* [make_opponents n deck] makes n opponents for the AI to face in simulations
out of the deck that is given to it *)
let rec make_opponents n deck =
match n with
|0 -> []
|n ->("simulation "^string_of_int n,
      (deal_hole deck))::make_opponents (n-1) deck

(* [preflop_odds c1 c2] is a simple algorithm for preflop odds we use to
determine the strength of a hand before the flop is dealt *)
let rec preflop_odds c1 c2 =
  let raw_odds =
    match (c1,c2) with
    | (x,y) when fst x = fst y && fst x<9 ->
        (if (float_of_int(fst(x)) +. 2.0)*.2.0 < 5.0 then 5.0
         else (float_of_int(fst(x)) +. 2.0)) /. 20.0
    | (x,y) when fst x = fst y ->
        (float_of_int(fst(x)) -. 2.0) /. 10.0
    | (x,y) ->
        let maxi = float_of_int(max (fst x) (fst y)) in
        if maxi > 9.0 then (maxi -. 2.0) /. 20.0
        else maxi /. 20.0
  in
  let suit_adjust = if snd c2 = snd c1 then raw_odds +. 0.1 else raw_odds in
  let gap = float_of_int(abs(fst c1 - fst c2)) /. 20.0 in
  let gap_adjust = if gap > 0.2 then suit_adjust -.0.2 else suit_adjust -.gap in
  let straight_adjust = if gap = 0.05 then gap_adjust +. gap else gap_adjust in
  straight_adjust

(* [card_prob opps cards] determines the probability the AI will win a round
given the current cards it has access to*)
let rec card_prob opps cards =
  let rec simulation trials opps user_cards (num,denom)=
  (if trials = 0 then (num,denom) else
  let new_deck = make_deck () |> shuffle |> remove_cards (user_cards) in
  let hole_opp_cards=(make_opponents 8 new_deck) in
    let comm_cards =
      match user_cards with
      |h1::h2::[] -> flop new_deck |> turn new_deck |> river new_deck
      |h1::h2::t when List.length t = 3 -> turn new_deck t |> river new_deck
      |h1::h2::t when List.length t = 4 -> river new_deck t
      |h1::h2::t when List.length t = 5 -> t
      |_ -> failwith "Invalid"
    in
  let opp_player_cards =
    List.map (fun (x,y) -> (x,y@comm_cards)) hole_opp_cards in
  let final_user_cards =
    match user_cards with
      |h1::h2::[] -> user_cards
      |h1::h2::t when List.length t = 3 -> [h1;h2]
      |h1::h2::t when List.length t = 4 -> [h1;h2]
      |h1::h2::t when List.length t = 5 -> [h1;h2]
      |_ -> failwith "Invalid"
    in
  let final_player_cards =
    ("user",final_user_cards@comm_cards)::opp_player_cards in
  let round_winner =
    (try winner final_player_cards
    with
    |_ -> "user")
    in
  if round_winner = "user" then
  simulation (trials - 1) opps user_cards (num + 1,denom + 1)
  else
  simulation (trials - 1) opps user_cards (num,denom + 1)) in
let (wins,trials) = simulation 500 opps cards (0,0) in
let probability = float_of_int(wins) /. float_of_int(trials) in
probability


let rec probability_win (st) (pl) : float =
  let all_player_cards = make_player_cards st in
    let rec get_player_option pl_cards pl =
    match pl_cards with
    |[] -> None
    |(p,cds)::t -> if p = pl.id then Some (p,cds) else get_player_option t pl
  in let current_player_cards = get_player_option all_player_cards pl in
  let num_opps = num_active_players st in
  match current_player_cards with
  |Some(p,cds) -> card_prob num_opps cds
  |_ -> failwith "Invalid Game State"


(* [round num] rounds the float that the to_call and bet functions create
to the nearest 10 so as to create neat betting values *)
let round (num: float) : float =
    if (Pervasives.mod_float num 10.0 = 0.) then num
    else (Pervasives.ceil ((num/.10.0)-.1.0))*.10.0

(* [raise_by_confidence amount hs conf] Helper to clean up code for raising
base on hand strength and randomly generated metric for confidence *)
let raise_by_confidence amount hs conf =
  (float_of_int(amount)+.(float_of_int(amount)*.hs *.conf))

let to_call raise st pl =
  let pot_odds =
      float_of_int(raise) /. (float_of_int(raise) +. float_of_int(st.pot)) in

  if st.deal_number < 3 then
  (
      let h1::h2::_ = pl.hole_hand in
      let hand_strength = preflop_odds h1 h2 in
      let r_of_r = hand_strength /. (hand_strength +. pot_odds) in
      let () = Random.self_init () in
      let random_num = Random.int 101 in
      let confidence = Random.float 1.2 in
      if hand_strength < 0.3 && confidence < 0.9 then 0.0
      else if r_of_r < 0.45 && random_num < 82 then
        (float_of_int(raise))
      else
        if random_num < 50 then (float_of_int(raise)) else
      (raise_by_confidence raise hand_strength confidence)
          |> round
  )
  else
  (let hand_strength = probability_win st pl in
  let r_of_r = hand_strength /. (hand_strength +. pot_odds) in
  let () = Random.self_init () in
  let random_num = Random.int 101 in
  let confidence = Random.float 1.2 in
  if r_of_r < 0.3 then
    (if random_num<85 then 0.0 else
        (float_of_int(raise)))
  else if r_of_r < 0.34 then
    (if random_num<50 then 0.0 else
      if random_num<85 then float_of_int(raise) else
       (raise_by_confidence raise hand_strength confidence)
        |> round)
  else if r_of_r < 0.40 then
    (if random_num<30 then 0.0 else
      if random_num<80 then float_of_int(raise) else
        (raise_by_confidence raise hand_strength confidence)
        |> round)
  else if r_of_r < 0.44 then
    (if random_num<60 then float_of_int(raise) else
        (raise_by_confidence raise hand_strength confidence)
        |> round)
  else
    (if random_num<25 then float_of_int(raise) else
        (raise_by_confidence raise hand_strength confidence)
        |> round)
  )

let bet_amount (st:state) (pl:player) :float =
  let hand_strength = probability_win st pl in
  let () = Random.self_init () in
  let confidence = Random.float 1.2 in
  let base = float_of_int(pl.money) in
    if st.deal_number < 3 then
      let h1::h2::_ = pl.hole_hand in
      let preflop_prb = preflop_odds h1 h2 in
      if preflop_prb < 0.4 && confidence < 1.0 then
      0.0
      else if preflop_prb < 0.5 then
      (base *. (Random.float 0.5) *. confidence)|>round
      else if preflop_prb < 0.75 then
      (base *. (Random.float 0.65) *. confidence)|>round
    else
      (base *. (Random.float 0.75) *. confidence)|>round
  else
    if hand_strength < 0.23 then 0.0 else
    let base = float_of_int(pl.money) in
      if hand_strength >= 0.7 && confidence >0.8 then
        float_of_int(pl.money)
      else if (base *. hand_strength *. confidence)>float_of_int(pl.money) then
        ((float_of_int(pl.money) *. hand_strength *. confidence)) |> round
      else if (base *. hand_strength *. confidence)<float_of_int(st.s_blind)then
        (((base +. float_of_int(st.s_blind)) *. hand_strength *. confidence))
        |> round
      else
        (base *. hand_strength *. confidence) |> round



