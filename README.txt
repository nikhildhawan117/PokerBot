==================================
PokerBot
==================================

Table of contents:
 1. Authors
 2. Install
 3. Acknowledgements
 4. Future

1. Authors
==================================


Amber Wiens, Nikhil Dhawan, Amritansh (Amrit) Kwatra


2. Install
==================================

The project has no external dependencies and only calls on packages that are included in OCaml 4.03. However, to ensure that our Unicode and ANSITerminal functions are properly called, if you think you do not have Camomile and ANSITerminal installed please first install those as described below:

To install Camomile:

opam install camomile

To install ANSITerminal:

1.Install package from: http://forge.ocamlcore.org/frs/download.php/1206/ANSITerminal-0.6.5.tar.gz
2. Uncompress the source archive and go to the root of the package
3. Run 'ocaml setup.ml -configure'
4. Run 'ocaml setup.ml -build'
5. Run 'ocaml setup.ml -install'

To run Project: To run our project, open/unzip the src file. Navigate to the src directory in bash and Run 'make play'



3. Acknowledgements
==================================

We'd like to thank CS 3110 TA Shiyu Wang and our friends for play testing this
game for us, and for sticking through testing despite extreme buggi-ness in early versions.

Lastly, we'd like to thank Professor Clarkson for being an inspiring professor, and helping us grow as students as well as individuals. Facing every challenge that you have thrown at us in this class has, despite being often times frustrating, and incredible learning experience.


4. Future
==================================

These features are not supported but are considered for future extensions:

- GUI Support
- Expansion to being able to play against multiple AIs
- Inclusion of network connectivity so as to allow players to play with their friends over wifi
