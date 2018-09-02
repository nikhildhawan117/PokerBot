(* Entry point for game in make play, gets fed to th game's main function
that initiates the game_loop *)

let () =
  ANSITerminal.erase Screen;
	ANSITerminal.(print_string [red]
		"\n\nWelcome to Texas Hold'em Poker!!!!!!!\n");
	print_endline "At any point in the game you may type 'help' to receive instructions, or enter 'quit' to quit.\n";
	print_endline "Please enter your name.\n";
	print_endline "> ";
	Game.main ()

