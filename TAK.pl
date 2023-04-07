win(Board, Player) :- Board = [[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_]].
win(Board, Player) :- Board = [[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_].
win(Board, Player) :- Board = [_,_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_]].

win(Board, Player) :- Board = [[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],[Player|_],_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [[Player|_],[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],[Player|_],_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,[Player|_],[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],[Player|_]].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],_,_,_,[Player|_],[Player|_]].
win(Board, Player) :- Board = [_,_,_,_,_,_,_,_,_,[Player|_],[Player|_],[Player|_],[Player|_],[Player|_],_,_].

win(Board, Player) :- Board = [[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_,_,_,[Player|_],_,_,_].
win(Board, Player) :- Board = [[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_,_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_,_,_,[Player|_],_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_,_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_,_,_,[Player|_]].
win(Board, Player) :- Board = [_,_,_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_,_,_,[Player|_],_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_]].
win(Board, Player) :- Board = [_,_,_,[Player|_],_,_,_,[Player|_],_,_,[Player|_],[Player|_],_,_,[Player|_],_].

win(Board, Player) :- Board = [[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,_].
win(Board, Player) :- Board = [_,_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_]].
win(Board, Player) :- Board = [_,_,_,_,_,_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,_].
win(Board, Player) :- Board = [[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],_].
win(Board, Player) :- Board = [_,_,[Player|_],_,_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,_,[Player|_],_,_,_].
win(Board, Player) :- Board = [_,[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_],[Player|_],_,_,_,[Player|_]].
win(Board, Player) :- Board = [_,_,_,[Player|_],_,_,[Player|_],[Player|_],_,[Player|_],[Player|_],_,_,[Player|_],_,_].

display([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- write([A1,A2,A3,A4]),nl,write([B1,B2,B3,B4]),nl,write([C1,C2,C3,C4]),nl,write([D1,D2,D3,D4]),nl,nl.

move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [NewA1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A1,L), not(L = 8), append([Player], A1, NewA1).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,NewA2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A2,L), not(L = 8), append([Player], A2, NewA2).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,NewA3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A3,L), not(L = 8), append([Player], A3, NewA3).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,NewA4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A4,L), not(L = 8), append([Player], A4, NewA4).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,NewB1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B1,L), not(L = 8), append([Player], B1, NewB1).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,NewB2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B2,L), not(L = 8), append([Player], B2, NewB2).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,NewB3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B3,L), not(L = 8), append([Player], B3, NewB3).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,NewB4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B4,L), not(L = 8), append([Player], B4, NewB4).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,NewC1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C1,L), not(L = 8), append([Player], C1, NewC1).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,NewC2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C2,L), not(L = 8), append([Player], C2, NewC2).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,NewC3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C3,L), not(L = 8), append([Player], C3, NewC3).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,NewC4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C4,L), not(L = 8), append([Player], C4, NewC4).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,NewD1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(D1,L), not(L = 8), append([Player], D1, NewD1).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,NewD2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(D2,L), not(L = 8), append([Player], D2, NewD2).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,NewD3,D4]) :- Rem > 0, Remain = Rem - 1, length(D3,L), not(L = 8), append([Player], D3, NewD3).
move([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], Player, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,NewD4]) :- Rem > 0, Remain = Rem - 1, length(D4,L), not(L = 8), append([Player], D4, NewD4).

pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], a1, Rem, Remain, [NewA1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A1,L), not(L = 8), append([black], A1, NewA1).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], a2, Rem, Remain, [A1,NewA2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A2,L), not(L = 8), append([black], A2, NewA2).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], a3, Rem, Remain, [A1,A2,NewA3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A3,L), not(L = 8), append([black], A3, NewA3).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], a4, Rem, Remain, [A1,A2,A3,NewA4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(A4,L), not(L = 8), append([black], A4, NewA4).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], b1, Rem, Remain, [A1,A2,A3,A4,NewB1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B1,L), not(L = 8), append([black], B1, NewB1).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], b2, Rem, Remain, [A1,A2,A3,A4,B1,NewB2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B2,L), not(L = 8), append([black], B2, NewB2).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], b3, Rem, Remain, [A1,A2,A3,A4,B1,B2,NewB3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B3,L), not(L = 8), append([black], B3, NewB3).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], b4, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,NewB4,C1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(B4,L), not(L = 8), append([black], B4, NewB4).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], c1, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,NewC1,C2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C1,L), not(L = 8), append([black], C1, NewC1).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], c2, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,NewC2,C3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C2,L), not(L = 8), append([black], C2, NewC2).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], c3, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,NewC3,C4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C3,L), not(L = 8), append([black], C3, NewC3).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], c4, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,NewC4,D1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(C4,L), not(L = 8), append([black], C4, NewC4).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], d1, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,NewD1,D2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(D1,L), not(L = 8), append([black], D1, NewD1).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], d2, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,NewD2,D3,D4]) :- Rem > 0, Remain = Rem - 1, length(D2,L), not(L = 8), append([black], D2, NewD2).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], d3, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,NewD3,D4]) :- Rem > 0, Remain = Rem - 1, length(D3,L), not(L = 8), append([black], D3, NewD3).
pmove([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], d4, Rem, Remain, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,NewD4]) :- Rem > 0, Remain = Rem - 1, length(D4,L), not(L = 8), append([black], D4, NewD4).
pmove(Board, _, _, _, Board) :- write('Illegal move.'), nl.

player_can_win_in_one(Board, Rem) :- move(Board, black, Rem, _, NewBoard), win(NewBoard, black).

respond(Board, _, Rem, Remain, NewBoard) :- move(Board, white, Rem, Remain, NewBoard), win(NewBoard, white), !.
respond(Board, Prem, Rem, Remain, NewBoard) :- move(Board, white, Rem, Remain, NewBoard), not(player_can_win_in_one(NewBoard, Prem)).
respond(Board, _, Rem, Remain, NewBoard) :- move(Board, white, Rem, Remain, NewBoard).
respond(Board, _, Rem, _, NewBoard) :- Rem = 0, !, write('YSOB, Im out'), nl, NewBoard = Board.

play :- explain, playfrom([[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]], 20, 20).

explain :-
  write('You play black by entering positions followed by a period.'),
  nl,
  display([a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4]).

playfrom(Board, _, _) :- win(Board, black), write('You win!').
playfrom(Board, _, _) :- win(Board, white), write('I win!').
playfrom(_, Prem, _) :- Prem = 0, write('Draw').
playfrom(_, _, Crem) :- Crem = 0, write('Draw').
playfrom(Board, Prem, Crem) :- read(N),
  pmove(Board, N, Prem, NewPrem, Newboard),
  display(Newboard),
  respond(Newboard, NewPrem, Crem, NewCrem, Newnewboard),
  display(Newnewboard),
  playfrom(Newnewboard, NewPrem, NewCrem).
