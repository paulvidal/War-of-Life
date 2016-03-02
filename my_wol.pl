
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
  NewN is N-1,
  statistics(runtime, [BeforeTime| _]),
  play(quiet, S1, S2, NumMoves, WinningPlayer),
  statistics(runtime, [AfterTime| _]),
  TimeTaken is AfterTime - BeforeTime,

  (
    (NewWinsP1 is WinsP1 + 1,
     WinningPlayer = 'b');

    (NewWinsP1 is WinsP1,
     WinningPlayer \= 'b')
  ),

  (
    (NewWinsP2 is WinsP2 + 1,
     WinningPlayer = 'r');

    (NewWinsP2 is WinsP2,
     WinningPlayer \= 'r')
  ),

  (
    (NewDraws is Draws+1,
     (
       WinningPlayer = 'exhaust';
       WinningPlayer = 'stalemate';
       WinningPlayer = 'draw'
     )
    );

    (NewDraws is Draws,
     (
       WinningPlayer \= 'exhaust';
       WinningPlayer \= 'stalemate';
       WinningPlayer \= 'draw'
     )
    )
  ),

  (
    (NewLongestGame is NumMoves,
     LongestGame < NumMoves,
     WinningPlayer \= 'exhaust');

    (NewLongestGame is LongestGame,
     NumMoves =< LongestGame)
  ),

  (
    (NewShortestGame is NumMoves,
     NumMoves < ShortestGame);

    (NewShortestGame is ShortestGame,
     ShortestGame =< NumMoves)
  ),

  NewTotalGameLength is TotalGameLength + NumMoves,
  NewTotalGameTime is TotalGameTime + TimeTaken,
  print_progression(NewN, TotalGames),
  simulate_games(NewN, S1, S2, NewWinsP1, NewWinsP2, NewDraws, NewLongestGame,
    NewShortestGame, NewTotalGameLength, NewTotalGameTime, TotalGames).

% print_progression is a helper function printing on the sreen the number
% of games left to play
print_progression(N, TotalGames) :-
  format('~w out of ~w~n', [N, TotalGames]).

% ---------------------------- BLOODLUST ---------------------------------

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_configs_for_player(PlayerColour, CurrentBoardState, AllConfigs),
  [FstConfig | Configs] = AllConfigs,
  [_, BoardStateAfterMove] = FstConfig,
  compute_score(bloodlust, PlayerColour, BoardStateAfterMove, Score),
  get_bloodlust_best_config(PlayerColour, CurrentBoardState,
      Configs, Score, FstConfig, [Move, NewBoardState]).

get_bloodlust_best_config(_, _, [], _, BestConfig, BestConfig).
get_bloodlust_best_config(PlayerColour, CurrentBoardState,
    [Config | Configs], CurrentBestScore, CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(bloodlust, PlayerColour, BoardStateAfterMove, Score),
  (
    (Score > CurrentBestScore,
     get_bloodlust_best_config(PlayerColour, CurrentBoardState,
         Configs, Score, Config, BestConfig)
    );

    (Score =< CurrentBestScore,
     get_bloodlust_best_config(PlayerColour, CurrentBoardState,
         Configs, CurrentBestScore, CurrentBestConfig, BestConfig)
    )
  ).

% ------------------------ SELF PRESERVATION -------------------------------

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_configs_for_player(PlayerColour, CurrentBoardState, AllConfigs),
  [FstConfig | Configs] = AllConfigs,
  [_, BoardStateAfterMove] = FstConfig,
  compute_score(self_preservation, PlayerColour, BoardStateAfterMove, Score),
  get_self_preservation_best_config(PlayerColour, CurrentBoardState,
      Configs, Score, FstConfig, [Move, NewBoardState]).

get_self_preservation_best_config(_, _, [], _, BestConfig, BestConfig).
get_self_preservation_best_config(PlayerColour, CurrentBoardState,
    [Config | Configs], CurrentBestScore, CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(self_preservation, PlayerColour, BoardStateAfterMove, Score),
  (
    (Score > CurrentBestScore,
     get_self_preservation_best_config(PlayerColour, CurrentBoardState,
         Configs, Score, Config, BestConfig)
    );

    (Score =< CurrentBestScore,
     get_self_preservation_best_config(PlayerColour, CurrentBoardState,
         Configs, CurrentBestScore, CurrentBestConfig, BestConfig)
    )
  ).

% ------------------------ LAND GRAB -------------------------------

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_configs_for_player(PlayerColour, CurrentBoardState, AllConfigs),
  [FstConfig | Configs] = AllConfigs,
  [_, BoardStateAfterMove] = FstConfig,
  compute_score(land_grab, PlayerColour, BoardStateAfterMove, Score),
  get_land_grab_best_config(PlayerColour,
      Configs, Score, FstConfig, [Move, NewBoardState]).

get_land_grab_best_config(_, [], _, BestConfig, BestConfig).
get_land_grab_best_config(PlayerColour,
    [Config | Configs], CurrentBestScore, CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(land_grab, PlayerColour, BoardStateAfterMove, Score),
  (
    (Score > CurrentBestScore,
     get_land_grab_best_config(PlayerColour,
         Configs, Score, Config, BestConfig)
    );

    (Score =< CurrentBestScore,
     get_land_grab_best_config(PlayerColour,
         Configs, CurrentBestScore, CurrentBestConfig, BestConfig)
    )
  ).

% --------------------------- MINIMAX ------------------------------------

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  perform_max(PlayerColour, CurrentBoardState, NewBoardState, Move).

perform_max(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_configs_for_player(PlayerColour, CurrentBoardState, AllConfigs),
  [FstConfig | Configs] = AllConfigs,
  [_, BoardStateAfterMove] = FstConfig,
  (
    (PlayerColour == 'b',
     NextPlayerColour = 'r'
    );

    (PlayerColour == 'r',
     NextPlayerColour = 'b'
    )
  ),
  next_generation(BoardStateAfterMove, BoardStateAfterGeneration),
  get_min(NextPlayerColour, BoardStateAfterGeneration, Score),
  get_max(NextPlayerColour, Configs, Score, FstConfig, [Move, NewBoardState]).

% We take the minimal score of the opponent that we can get out of a move,
% that maximises our chances off wining.
get_max(_, [], _, BestConfig, BestConfig).
get_max(PlayerColour, [Config | Configs], CurrentBestMinScore,
    CurrentBestConfig, BestConfig) :-
  [_, BoardStateAfterMove] = Config,
  next_generation(BoardStateAfterMove, BoardStateAfterGeneration),
  get_min(PlayerColour, BoardStateAfterGeneration, MinScore),
  (
    (MinScore < CurrentBestMinScore,
     get_max(PlayerColour, Configs, MinScore, Config, BestConfig)
    );

    (MinScore >= CurrentBestMinScore,
     get_max(PlayerColour, Configs, CurrentBestMinScore,
         CurrentBestConfig, BestConfig)
     )
  ).

% The opponent takes the score that minimises our chances by taking the maximum
% score out of the land-grab heuristic function
get_min('r', [_, []], -100).
get_min('b', [[], _], -100).
get_min(PlayerColour, BoardState, MinScore) :-
  get_all_configs_for_player(PlayerColour, BoardState, AllConfigs),
  [FstConfig | Configs] = AllConfigs,
  [_, BoardStateAfterMove] = FstConfig,
  compute_score(land_grab, PlayerColour, BoardStateAfterMove, Score),
  get_min_score(PlayerColour, Configs, Score, MinScore).

get_min_score(_, [], MinScore, MinScore).
get_min_score(PlayerColour, [Config | Configs], CurrentMinScore, MinScore) :-
  [_, BoardStateAfterMove] = Config,
  compute_score(land_grab, PlayerColour, BoardStateAfterMove, Score),
  (
    (Score > CurrentMinScore,
     get_min_score(PlayerColour, Configs, Score, MinScore)
    );

    (Score =< CurrentMinScore,
     get_min_score(PlayerColour, Configs, CurrentMinScore, MinScore)
    )
  ).

% --------------------------- COMPUTE SCORE -----------------------------------

compute_score(Strategy, 'b', [Pieces, OpponentPieces], Score) :-
  compute_score(Strategy, [Pieces, OpponentPieces], Score).
compute_score(Strategy, 'r', [OpponentPieces, Pieces], Score) :-
  compute_score(Strategy, [Pieces, OpponentPieces], Score).

compute_score(bloodlust, BoardStateAfterMove,  Score) :-
  [_, OpponentPiecesBeforeMove] = BoardStateAfterMove,
  next_generation(BoardStateAfterMove, NewBoardState),
  [_, OpponentPiecesAfterGeneration] = NewBoardState,
  compute_pieces_difference(OpponentPiecesBeforeMove,
      OpponentPiecesAfterGeneration, Score).
compute_score(self_preservation, BoardStateAfterMove,  Score) :-
  [PiecesAfterMove, _] = BoardStateAfterMove,
  next_generation(BoardStateAfterMove, NewBoardState),
  [PlayerPiecesAfterGeneration, _] = NewBoardState,
  compute_pieces_difference(PlayerPiecesAfterGeneration,
      PiecesAfterMove, Score).
compute_score(land_grab, BoardStateAfterMove,  Score) :-
  next_generation(BoardStateAfterMove, NewBoardState),
  [PlayerPiecesAfterGeneration, OpponentPiecesAfterGeneration] = NewBoardState,
  compute_pieces_difference(PlayerPiecesAfterGeneration,
      OpponentPiecesAfterGeneration, Score).

compute_pieces_difference(PlayerPieces1, PlayerPieces2, Score) :-
  length(PlayerPieces1, NumPlayer1Pieces),
  length(PlayerPieces2, NumPlayer2Pieces),
  Score is NumPlayer1Pieces - NumPlayer2Pieces.

% ------------------------ CREATE ALL CONFIGS ---------------------------------

% Gets all the possible configs obtainable for a specific player given a board.
% A CONFIGURATION is a 2 list element consisting of a move represented as
% [r1,c1,r2,c2] where (r1,c1) represent the piece position before moving and
% (r2,c2) after moving. The second element is the new board obtained
% after performing the move.

get_all_configs_for_player('b', [PlayerPieces, OpponentPieces], AllConfigs) :-
  findall([Move, [NewPlayerPieces, OpponentPieces]],
          perform_moves(PlayerPieces, OpponentPieces, Move, NewPlayerPieces),
	        AllConfigs).
get_all_configs_for_player('r', [OpponentPieces, PlayerPieces], AllConfigs) :-
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
