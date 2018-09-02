open OUnit2
open Game
open AI
open State
open Player
open Card
open Ranker
open Dealer

let cards1=[(0,Card.Hearts);(8, Card.Diamonds);(12,Card.Spades);(2,Card.Clubs)]
let cards_exn1=(13,Card.Hearts)
let cards_exn2= ((-1),Card.Hearts)

let test_card_module =
[

   "standard_cards" >:: (fun _ -> assert_equal
    ["2♥";"10♦";"A♠";"4♣"]
    (List.map (Card.card_to_string) cards1));
   "card exn test1" >:: (fun _ -> assert_raises
        (Failure("Invalid Rank")) (fun () -> Card.card_to_string cards_exn1));
   "card exn test2" >:: (fun _ -> assert_raises
        (Failure("Invalid Rank")) (fun () -> Card.card_to_string cards_exn2));

]

let test_dealer_module =
[
  "base_deck" >:: (fun _ -> assert_equal
  {Dealer.crnt_card = 0; cards =
  [|(0, Card.Diamonds); (1, Card.Diamonds); (2, Card.Diamonds);
    (3, Card.Diamonds); (4, Card.Diamonds); (5, Card.Diamonds);
    (6, Card.Diamonds); (7, Card.Diamonds); (8, Card.Diamonds);
    (9, Card.Diamonds); (10, Card.Diamonds); (11, Card.Diamonds);
    (12, Card.Diamonds); (0, Card.Hearts); (1, Card.Hearts); (2, Card.Hearts);
    (3, Card.Hearts); (4, Card.Hearts); (5, Card.Hearts); (6, Card.Hearts);
    (7, Card.Hearts); (8, Card.Hearts); (9, Card.Hearts); (10, Card.Hearts);
    (11, Card.Hearts); (12, Card.Hearts); (0, Card.Clubs); (1, Card.Clubs);
    (2, Card.Clubs); (3, Card.Clubs); (4, Card.Clubs); (5, Card.Clubs);
    (6, Card.Clubs); (7, Card.Clubs); (8, Card.Clubs); (9, Card.Clubs);
    (10, Card.Clubs); (11, Card.Clubs); (12, Card.Clubs); (0, Card.Spades);
    (1, Card.Spades); (2, Card.Spades); (3, Card.Spades); (4, Card.Spades);
    (5, Card.Spades); (6, Card.Spades); (7, Card.Spades); (8, Card.Spades);
    (9, Card.Spades); (10, Card.Spades); (11, Card.Spades); (12, Card.Spades)|]}
    (Dealer.make_deck ()));
  "deal_hole_1" >:: (fun _ -> assert_equal
    [(0, Card.Diamonds); (1, Card.Diamonds)]
    (let d = Dealer.make_deck () in Dealer.deal_hole d ));
  "deal_hole_2" >:: (fun _ -> assert_equal
    [(2, Card.Diamonds); (3, Card.Diamonds)]
    (let d = Dealer.make_deck () in let hole_1 = Dealer.deal_hole d in
    Dealer.deal_hole d));
  "deal_flop" >:: (fun _ -> assert_equal
    [(5, Card.Diamonds); (6, Card.Diamonds); (7, Card.Diamonds)]
    (let d = Dealer.make_deck () in let hole_1 = Dealer.deal_hole d in
    let hole_2=Dealer.deal_hole d in Dealer.flop d));
  "deal_turn" >:: (fun _ -> assert_equal
    [(9, Card.Diamonds);(5, Card.Diamonds);(6, Card.Diamonds);(7,Card.Diamonds)]
    (let d = Dealer.make_deck () in let hole_1 = Dealer.deal_hole d in
    let hole_2=Dealer.deal_hole d in let flop= Dealer.flop d in
    Dealer.turn d flop));
  "deal_river" >::(fun _ -> assert_equal
    [(11, Card.Diamonds); (9, Card.Diamonds); (5, Card.Diamonds);
      (6, Card.Diamonds); (7, Card.Diamonds)]
    (let d = Dealer.make_deck () in let hole_1 = Dealer.deal_hole d in
    let hole_2=Dealer.deal_hole d in let flop= Dealer.flop d in
    let turn = Dealer.turn d flop in Dealer.river d turn));
  "shuffle" >:: (fun _ -> assert_equal false ((Dealer.make_deck() |> shuffle) =
    (Dealer.make_deck() |> shuffle)))
]
let d = Dealer.make_deck ()
let p1 = {id = "test1" ; hole_hand =[(0, Card.Diamonds); (1, Card.Diamonds)];
              money= 5000; pot_money= 0; is_active= true; has_raised=false;
              is_AI= false; last_cmd= None}
let p2 = {id = "test2" ; hole_hand =[(2, Card.Diamonds); (3, Card.Diamonds)];
              money= 5000; pot_money= 0; is_active= true; has_raised=false;
              is_AI= true; last_cmd= None}

let test_player_module =
[
  "test make_player1" >:: (fun _ -> assert_equal p1
  (Player.make_player "test1" false d));
  "test valid_bet1" >:: (fun _ -> assert_equal true (Player.valid_bet p1 4000));
  "test valid_bet2" >:: (fun _ -> assert_equal false (Player.valid_bet p1 8000));
  "test valid_bet3" >:: (fun _ -> assert_equal true (Player.valid_bet p1 5000));
  "test outlier_bet1" >:: (fun _ -> assert_equal false (Player.valid_bet p1 (-100)));
  "test adjust_money" >:: (fun _ -> assert_equal {p1 with money = 5010} (Player.adjust_money p1 10));
  "test adjust_money1" >:: (fun _ -> assert_equal {p1 with money = 5010} (Player.adjust_money p1 10));
  "test adjust_money2" >:: (fun _ -> assert_equal {p1 with money = 4990} (Player.adjust_money p1 (-10)));
]

let st1 = {round_number=0; deal_number=0; deck = d; b_blind = 100;
            s_blind = 50; players= [p1;p2];
            curr_bet=0; pot=0; comm_cards=[(7, Card.Clubs); (4, Card.Spades);
            (5, Card.Clubs);(6, Card.Clubs); (12, Card.Diamonds)]}
let st2 = {st1 with comm_cards = [(7, Card.Clubs); (10, Card.Spades);
            (5, Card.Clubs);(6, Card.Clubs); (12, Card.Diamonds)]}
let p3 = {p2 with hole_hand =[(11,Card.Hearts);(0,Card.Diamonds)]}
let st3 = {st2 with players=[p1;p3]}
let p4 = {p1 with hole_hand =[(7,Card.Hearts);(0,Card.Diamonds)]}
let st4 = {st3 with players=[p3;p4]}
let p5 = {p2 with hole_hand = [(7,Card.Clubs);(10,Card.Diamonds)]}
let st5 = {st3 with players=[p4;p5]}
let st6 = {st5 with comm_cards = [(0, Card.Clubs); (0, Card.Spades);
            (7, Card.Diamonds);(10, Card.Clubs); (12, Card.Diamonds)]}
let st7 = {st5 with comm_cards = [(8, Card.Clubs); (9, Card.Spades);
            (6, Card.Diamonds);(2, Card.Clubs); (12, Card.Diamonds)]}
let st8 = {st5 with comm_cards = [(8, Card.Clubs); (9, Card.Clubs);
            (6, Card.Clubs);(2, Card.Clubs); (12, Card.Diamonds)]}
let st9 = {st5 with comm_cards = [(7, Card.Diamonds); (5, Card.Spades);
            (0, Card.Clubs);(0, Card.Spades); (12, Card.Diamonds)]}
let st10 = {st5 with comm_cards = [(7, Card.Diamonds); (0, Card.Hearts);
            (0, Card.Clubs);(0, Card.Spades); (12, Card.Diamonds)]}
let p6 = {p1 with hole_hand =[(7,Card.Clubs);(10,Card.Clubs)]}
let p7 = {p2 with hole_hand =[(7,Card.Spades);(10,Card.Diamonds)]}
let st11 = {st5 with comm_cards = [(8, Card.Clubs); (9, Card.Clubs);
            (6, Card.Clubs);(2, Card.Diamonds); (12, Card.Hearts)];
            players = [p6;p7] }

let test_ranker_module =
[
  "test_winner1" >:: (fun _ -> assert_equal "test2"
      (Ranker.make_player_cards st1 |> winner));
  "test_exn_raise" >:: (fun _ -> assert_raises (Ranker.Tie)
      (fun () -> Ranker.make_player_cards st2 |> winner ));
  "test_high_card win" >:: (fun _ -> assert_equal "test2"
      (Ranker.make_player_cards st3 |> winner));
  "test_pair_win" >:: (fun _ -> assert_equal "test1"
      (Ranker.make_player_cards st4 |> winner));
  "test_two_pair_win" >:: (fun _ -> assert_equal "test2"
      (Ranker.make_player_cards st5 |> winner));
  "test_three_of_a_kind_win" >:: (fun _ -> assert_equal "test1"
      (Ranker.make_player_cards st6 |> winner));
  "test_straight_win" >:: (fun _ -> assert_equal "test2"
      (Ranker.make_player_cards st7 |> winner));
  "test_flush_win" >:: (fun _ -> assert_equal "test2"
      (Ranker.make_player_cards st8 |> winner));
  "test_full_house_win" >:: (fun _ -> assert_equal "test1"
    (Ranker.make_player_cards st9 |> winner));
  "test_four_of_a_kind_win" >:: (fun _ -> assert_equal "test1"
    (Ranker.make_player_cards st10 |> winner));
  "test_straight_flush_win" >:: (fun _ -> assert_equal "test1"
      (Ranker.make_player_cards st11 |> winner));

]

let st = {round_number = 1; deal_number = 0; deck = d; b_blind = 100;
  s_blind = 50; players = [p1;p2]; curr_bet = 0; pot = 0;
  comm_cards = []}


let test_game_module =
[
  "test_make_state" >:: (fun _ -> assert_equal st (Game.new_state [p1;p2] d));
  "test_max_bid" >:: (fun _ -> assert_equal 5000 (Game.max_bid st));
  "test_raise" >:: (fun _ -> assert_equal 4950
      (Game.max_bid (Game.do' (Raise 50) p1 st)));
  "test_raise_pot" >:: (fun _ -> assert_equal 50
      (Game.do' (Raise 50) p1 st).pot);
  "test_raise_current_bet" >:: (fun _ -> assert_equal 50
      (Game.do' (Raise 50) p1 st).curr_bet);
  "test_raise_has_raised" >:: (fun _-> assert_equal true
      ((List.hd ((Game.do' (Raise 50) p1 st).players)).has_raised));
  "test_bet" >:: (fun _ -> assert_equal 4950
      (Game.max_bid (Game.do' (Bet 50) p1 st)));
  "test_bet_pot" >:: (fun _ -> assert_equal 50
      (Game.do' (Bet 50) p1 st).pot);
  "test_bet_current_bet" >:: (fun _ -> assert_equal 50
      (Game.do' (Bet 50) p1 st).curr_bet);
  "test_fold" >:: (fun _ -> assert_equal 5000
      (Game.max_bid (Game.do' (Fold) p1 st)));
  "test_fold_empty_pot" >:: (fun _ -> assert_equal 0
      (Game.do' (Fold) p1 st).pot);
  "test_fold_player_active">:: (fun _ -> assert_equal false
      (((List.rev(Game.do' (Fold) p2 st).players) |> List.hd).is_active));
  "test_nonchanging_quit" >:: (fun _ -> assert_equal st
      (Game.do' (Quit) p1 st));
  "test_nonchanging_help" >:: (fun _ -> assert_equal st
      (Game.do' (Help) p1 st));
  "test_call" >:: (fun _ -> assert_equal 100 ((Game.do'(Raise 50) p1 st |>
        Game.do' Call p2).pot));

]

let suite =
	"poker test suite"
	>:::test_card_module@test_dealer_module@test_player_module@test_ranker_module
    @test_game_module

let _ = run_test_tt_main suite