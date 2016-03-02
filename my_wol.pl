
% test_strategy prints out the statistics after performing N games,
% using strategyP1 as the first Player strategy and
% using strategyP2 as the second Player strategy
test_strategy(N, StrategyP1, StrategyP2) :-
  N > 0,
  simulate_games(N, StrategyP1, StrategyP2, 0, 0, 0, 0, 250, 0, 0, N).

% simulate_games is the helper function for test_strategy that computes all
% the statictis necessary
simulate_games(0, _, _, WinsP1, WinsP2, Draws, LongestGame, ShortestGame,
        TotalGameLength, TotalGameTime, TotalGames) :-
  format('Number of Draws: ~w~n', [Draws]),
  format('Number of wins for Player 1 (blue): ~w~n', [WinsP1]),
  format('Number of wins for Player 2 (red): ~w~n', [WinsP2]),
  format('Longest (non-exhaustive) game: ~w moves~n', [LongestGame]),
  format('Shortest Game : ~w moves~n', [ShortestGame]),
  AvgGameLengthStat is TotalGameLength / TotalGames,
  format('Average game length : ~w moves~n', [AvgGameLengthStat]),
  AvgGameTimeStat is TotalGameTime / TotalGames,
  format('Average game time: ~w ms~n', [AvgGameTimeStat]), !.

simulate_games(N, S1, S2, WinsP1, WinsP2, Draws, LongestGame, ShortestGame,
        TotalGameLength, TotalGameTime, TotalGames) :-
  % Decrement the game count
  NewN is N-1,

  % Plays the game
  play_game(S1, S2, NumMoves, WinningPlayer, TimeTaken),

  % Update the scores
  new_player_score(WinningPlayer, 'b', WinsP1, NewWinsP1),
  new_player_score(WinningPlayer, 'r', WinsP2, NewWinsP2),

  % Update the the draw count
  update_draws(WinningPlayer, Draws, NewDraws),

  % Get the longest game
  get_longest_game(WinningPlayer, NumMoves, LongestGame, NewLongestGame),

  % Get the shortest game
  get_shortest_game(NumMoves, ShortestGame, ShortestGame),

  % Get the new total game length and time
  NewTotalGameLength is TotalGameLength + NumMoves,
  NewTotalGameTime is TotalGameTime + TimeTaken,

  print_progression(NewN, TotalGames),

  % Continue recursively to simulate game until the base case is reached
  simulate_games(NewN, S1, S2, NewWinsP1, NewWinsP2, NewDraws, NewLongestGame,
    NewShortestGame, NewTotalGameLength, NewTotalGameTime, TotalGames).

% Play a game and return the game statistics
play_game(S1, S2, NumMoves, WinningPlayer, TimeTaken) :-
  statistics(runtime, [BeforeTime| _]),
  play(quiet, S1, S2, NumMoves, WinningPlayer),
  statistics(runtime, [AfterTime| _]),
  TimeTaken is AfterTime - BeforeTime.

% Calculate new player's score depending on the winning player
new_player_score('r', 'b', Wins, Wins).
new_player_score('b', 'r', Wins, Wins).
new_player_score('b', 'b', Wins, NewWins) :-
  NewWins is Wins + 1.
new_player_score('r', 'r', Wins, NewWins) :-
  NewWins is Wins + 1.

% Calculate the new draw count depending on the game result
update_draws(WinningPlayer, Draws, NewDraws) :-
  is_draw(WinningPlayer),
  NewDraws is Draws + 1.
update_draws(WinningPlayer, Draws, Draws) :-
  \+ is_draw(WinningPlayer).

is_draw('exhaust').
is_draw('stalemate').
is_draw('draw').

% Get the longest non exhaustive game in term of moves
get_longest_game(WinningPlayer, NumMoves, LongestGame, NumMoves) :-
  WinningPlayer \= 'exhaust',
  LongestGame < NumMoves.
get_longest_game(WinningPlayer, NumMoves, LongestGame, LongestGame) :-
  LongestGame >= NumMoves.

% Get the shortest game in term of moves
get_shortest_game(NumMoves, ShortestGame, NumMoves) :-
  ShortestGame > NumMoves.
get_shortest_game(NumMoves, ShortestGame, ShortestGame) :-
  ShortestGame =< NumMoves.

% print_progression is a helper function printing on the sreen the number
% of games left to play
print_progression(N, TotalGames) :-
  format('~w out of ~w~n', [N, TotalGames]).

% ---------------------------- BLOODLUST -----------------------------------

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_best_config(bloodlust, PlayerColour, CurrentBoardState, [
      Move, NewBoardState]).

% ------------------------ SELF PRESERVATION -------------------------------

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_best_config(self_preservation, PlayerColour, CurrentBoardState, [
      Move, NewBoardState]).

% ---------------------------- LAND GRAB ------------------------------------

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_best_config(land_grab, PlayerColour, CurrentBoardState, [
      Move, NewBoardState]).

% --------------------------- BEST CONFIG -----------------------------------

get_best_config(Strategy, PlayerColour, CurrentBoardState, BestConfig) :-
  get_all_configs(PlayerColour, CurrentBoardState, AllConfigs),
  get_best_config(Strategy, PlayerColour, CurrentBoardState, AllConfigs, -100,
      [], BestConfig).

get_best_config(_, _, _, [], _, BestConfig, BestConfig).
get_best_config(Strategy, PlayerColour, CurrentBoardState, [Config | Configs],
    CurrentBestScore, CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(Strategy, PlayerColour, BoardStateAfterMove, Score),
  get_current_best_score_and_config(Score, CurrentBestScore, Config,
      CurrentBestConfig, NewCurrentBestScore, NewCurrentBestConfig),
  get_best_config(Strategy, PlayerColour, CurrentBoardState,
      Configs, NewCurrentBestScore, NewCurrentBestConfig, BestConfig).

get_current_best_score_and_config(Score, CurrentBestScore, Config,
    _, NewCurrentBestScore, NewCurrentBestConfig) :-
  Score > CurrentBestScore,
  NewCurrentBestScore = Score,
  NewCurrentBestConfig = Config.
get_current_best_score_and_config(Score, CurrentBestScore, _,
    CurrentBestConfig, NewCurrentBestScore, NewCurrentBestConfig) :-
  Score =< CurrentBestScore,
  NewCurrentBestScore = CurrentBestScore,
  NewCurrentBestConfig = CurrentBestConfig.

% ---------------------------------------------------------------------------
% --------------------------- MINIMAX ---------------------------------------

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  perform_max(PlayerColour, CurrentBoardState, [Move, NewBoardState]).

perform_max(PlayerColour, CurrentBoardState, BestConfig) :-
  get_all_configs(PlayerColour, CurrentBoardState, AllConfigs),
  swap_players(PlayerColour, NextPlayerColour),
  get_max(NextPlayerColour, AllConfigs, 100, [], BestConfig).

% We take the minimal score of the opponent that we can get out of a move,
% that maximises our chances off wining.
get_max(_, [], _, BestConfig, BestConfig).
get_max(PlayerColour, [Config | Configs], CurrentBestMinScore,
    CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  next_generation(BoardStateAfterMove, BoardStateAfterGeneration),
  perform_min(PlayerColour, BoardStateAfterGeneration,
      CurrentBestMinScore, MinScore),
  (
    (MinScore < CurrentBestMinScore,
     NewCurrentBestMinScore = MinScore,
     NewCurrentBestConfig = Config
    );

    (MinScore >= CurrentBestMinScore,
     NewCurrentBestMinScore = CurrentBestMinScore,
     NewCurrentBestConfig = CurrentBestConfig
    )
  ),
  get_max(PlayerColour, Configs, NewCurrentBestMinScore,
      NewCurrentBestConfig, BestConfig).

% The opponent takes the score that minimises our chances by taking the maximum
% score out of the land-grab heuristic function
perform_min(PlayerColour, BoardState, CurrentBestMinScore, MinScore) :-
  get_all_configs(PlayerColour, BoardState, AllConfigs),
  get_min(PlayerColour, AllConfigs, -100, CurrentBestMinScore, MinScore).

get_min(_, [], MinScore, _, MinScore).
get_min(PlayerColour, [Config | Configs], CurrentMinScore,
    CurrentBestMinScore, MinScore) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(land_grab, PlayerColour, BoardStateAfterMove, Score),
  (
    % alpha-beta prunning
    (Score >= CurrentBestMinScore,
     MinScore = 100
    );

    (Score < CurrentBestMinScore,
    ((Score > CurrentMinScore,
     get_min(PlayerColour, Configs, Score, CurrentBestMinScore, MinScore)
    );

    (Score =< CurrentMinScore,
     get_min(PlayerColour, Configs, CurrentMinScore, CurrentBestMinScore, MinScore)
    ))
    )
  ).

% Swap players in order to perform opponent move
swap_players('b', 'r').
swap_players('r', 'b').

% --------------------------- COMPUTE SCORE -----------------------------------

% Functions conputing the score based on the player and a given strategy

compute_score(Strategy, 'b', [Pieces, OpponentPieces], Score) :-
  compute_score(Strategy, [Pieces, OpponentPieces], Score).
compute_score(Strategy, 'r', [OpponentPieces, Pieces], Score) :-
  compute_score(Strategy, [Pieces, OpponentPieces], Score).

% Helper functions computing the score based on the chosen strategy

% bloodlust gives back a Score representing the difference between the
% Opponent pieces after generation and before generation
compute_score(bloodlust, BoardStateAfterMove,  Score) :-
  [_, OpponentPiecesBeforeMove] = BoardStateAfterMove,
  next_generation(BoardStateAfterMove, NewBoardState),
  [_, OpponentPiecesAfterGeneration] = NewBoardState,
  compute_pieces_difference(OpponentPiecesBeforeMove,
      OpponentPiecesAfterGeneration, Score).

% self_preservation gives back a Score representing the difference between the
% Player pieces after generation and before generation
compute_score(self_preservation, BoardStateAfterMove,  Score) :-
  [PiecesAfterMove, _] = BoardStateAfterMove,
  next_generation(BoardStateAfterMove, NewBoardState),
  [PlayerPiecesAfterGeneration, _] = NewBoardState,
  compute_pieces_difference(PlayerPiecesAfterGeneration,
      PiecesAfterMove, Score).

% self_preservation gives back a Score representing the difference between the
% Player pieces after generation and the Opponent pieces after generation
compute_score(land_grab, BoardStateAfterMove,  Score) :-
  next_generation(BoardStateAfterMove, NewBoardState),
  [PlayerPiecesAfterGeneration, OpponentPiecesAfterGeneration] = NewBoardState,
  compute_pieces_difference(PlayerPiecesAfterGeneration,
      OpponentPiecesAfterGeneration, Score).

% Helper functions computing the difference between 2 players' pieces and
% giving back the difference
compute_pieces_difference(PlayerPieces1, PlayerPieces2, Difference) :-
  length(PlayerPieces1, NumPlayer1Pieces),
  length(PlayerPieces2, NumPlayer2Pieces),
  Difference is NumPlayer1Pieces - NumPlayer2Pieces.

% ------------------------ CREATE ALL CONFIGS ---------------------------------

% Gets all the possible configs obtainable for a specific player given a board.
% A CONFIGURATION is a 2 list element consisting of a move represented as
% [r1,c1,r2,c2] where (r1,c1) represent the piece position before moving and
% (r2,c2) after moving. The second element is the new board obtained
% after performing the move.

get_all_configs('b', [PlayerPieces, OpponentPieces], AllConfigs) :-
  findall([Move, [NewPlayerPieces, OpponentPieces]],
          perform_moves(PlayerPieces, OpponentPieces, Move, NewPlayerPieces),
	        AllConfigs).
get_all_configs('r', [OpponentPieces, PlayerPieces], AllConfigs) :-
  findall([Move, [OpponentPieces, NewPlayerPieces]],
          perform_moves(PlayerPieces, OpponentPieces, Move, NewPlayerPieces),
	        AllConfigs).

perform_moves(PlayerPieces, OpponentPieces, Move, NewPlayerPieces) :-
  get_valid_moves(PlayerPieces, OpponentPieces, Move),
  alter_board(Move, PlayerPieces, NewPlayerPieces).

get_valid_moves(PlayerPieces, OpponentPieces, Move) :-
  member([A,B], PlayerPieces),
  neighbour_position(A,B,[NewA, NewB]),
  check_valid_move(NewA, NewB, PlayerPieces, OpponentPieces),
  Move = [A, B, NewA, NewB].

check_valid_move(NewA, NewB, PlayerPieces, OpponentPieces) :-
  \+ member([NewA,NewB], PlayerPieces),
  \+ member([NewA,NewB], OpponentPieces).

% DEBUGGING
print_configs([]).
print_configs([[Move, Board] | R]) :-
  format('~w~n~w~n~n', [Move, Board]),
  print_configs(R).
