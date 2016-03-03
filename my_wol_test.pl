run_test :-
  test_bloodlust_1,
  test_bloodlust_2,
  test_self_preservation,
  test_land_grab.

test_bloodlust_1 :-
  BluePieces = [ [2,2], [3,2], [3,3] ],
  RedPieces = [ [2,4], [3,4] ],
  StartingBoard = [BluePieces, RedPieces],
  bloodlust('b', StartingBoard, _, Move),
  Expected = [3,3,4,2],
  format('~nTest1~nExpected :~n~w~nFound:~n~w~n~n', [Expected, Move]).

test_bloodlust_2 :-
  BluePieces = [ [2,2] ],
  RedPieces = [ [1,1], [1,3], [2,3] ],
  StartingBoard = [BluePieces, RedPieces],
  bloodlust('b', StartingBoard, _, Move),
  Expected = [2,2,2,1],
  format('~nTest2~nExpected :~n~w~nFound:~n~w~n~n', [Expected, Move]).

test_self_preservation :-
  BluePieces = [ [1,3], [1,4], [2,1], [3,1], [4,4] ],
  RedPieces = [ ],
  StartingBoard = [BluePieces, RedPieces],
  self_preservation('b', StartingBoard, _, Move),
  Expected = [1,3,2,2],
  format('~nTest3~nExpected :~n~w~nFound:~n~w~n~n', [Expected, Move]).

test_land_grab :-
  BluePieces = [ [2,2], [3,1], [4,1] ],
  RedPieces = [ [1,3], [1,4] ],
  StartingBoard = [BluePieces, RedPieces],
  land_grab('b', StartingBoard, _, Move),
  Expected = [2,2,3,2],
  format('~nTest4~nExpected :~n~w~nFound:~n~w~n~n', [Expected, Move]).

test_minimax :-
  BluePieces = [ [2,2], [3,1], [4,1] ],
  RedPieces = [ [1,3], [1,4] ],
  StartingBoard = [BluePieces, RedPieces],
  minimax('b', StartingBoard, _, Move),
  Expected = [2,2,3,2],
  format('~nTest4~nExpected :~n~w~nFound:~n~w~n~n', [Expected, Move]).
