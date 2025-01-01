play :-
    display_menu,
    read_option(Option),
    handle_option(Option).


display_menu :-
    nl,
    write('#########################################'), nl,nl,
    write('                REPLICA                  '), nl,nl,
    write('1. Play Human vs Human'), nl,
    write('2. Play Human vs Computer'), nl,
    write('3. Play Computer vs Human'), nl,
    write('4. Play Computer vs Computer'), nl,
    write('5. Exit'), nl,nl,
    write('#########################################'), nl,
    write('Choose an option (1-5): '), nl.


read_option(Option) :-
    read(Option),
    integer(Option),
    Option >= 1,
    Option =< 5, !.
read_option(_) :-
    write('Invalid option. Please choose a number between 1 and 5.'), nl,
    display_menu,
    read_option(Option).

% Handles the chosen option from the menu
handle_option(1) :-
    write('Starting Human vs Human mode...'), nl,
    initial_state((human, human)).
handle_option(2) :-
    write('Starting Human vs Computer mode...'), nl,
    initial_state((human, computer)).
handle_option(3) :-
    write('Starting Computer vs Human mode...'), nl,
    initial_state((computer, human)).
handle_option(4) :-
    write('Starting Computer vs Computer mode...'), nl,
    initial_state((computer, computer)).
handle_option(5) :-
    write('Exiting the game. Goodbye!'), nl.

% Placeholder for starting the game with the given configurations
initial_state((Player1, Player2)) :-
    write('Game starting... (Player 1: '), write(Player1), write(', Player 2: '), write(Player2), write(')'), nl,
    display_game(board([
        ['x', 'b', 'b', 'b', '_', '_','_', '_'],
        ['b', 'b', 'b', 'b', '_', '_', '_', '_'],
        ['b', 'b', '_', '_','_', '_', '_', '_'],
        ['b', 'b', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', 'w', 'w'],
        ['_', '_', '_', '_', '_', '_', 'w', 'w'],
        ['_', '_', '_', '_',  'w', 'w', 'w', 'w'],
        ['_', '_', '_', '_',  'w', 'w', 'w', 'y']
    ]),Player1,w).
    

    board([
        [x, b, b, b, _, _, _, _],
        [b, b, b, b, _, _, _, _],
        [b, b, _, _, _, _, _, _],
        [b, b, _, _, _, _, _, _],
        [_, _, _, _, _, _, w, w],
        [_, _, _, _, _, _, w, w],
        [_, _, _, _, w, w, w, w],
        [_, _, _, _, w, w, w, y]
    ]).

% Display game for computer
display_game(board(Rows), computer) :-
    write('Computer is gonna play'), nl.

display_game(board(Rows), human, w) :-           % Display game for white, human
    write('White to play'), nl,
    display_board(board(Rows)), nl,
    write('Insert your play: '), nl,
    read_move(Play, w, board(Rows)),
    write('Good move!'), nl,
    move(board(Rows), w, Play, NewBoard),
    ( 
        game_over(NewBoard, Winner),
        (Winner = 'b' -> nl,write('Black is the winner !!!'), nl,nl;
         Winner = 'w' -> nl,write('White is the winner !!!'), nl,nl),
         display_board(NewBoard)
    ;
        display_game(NewBoard, human, b)        % Continue the game if no winner
    ).

display_game(board(Rows), human,b) :-        %display game for black, human
    write('Black to play'), nl,
    display_board(board(Rows)), nl,
    write('Insert your play: '),nl,
    read_move(Play,b,board(Rows)),
    write('good move!'),nl,
    move(board(Rows),b,Play,NewBoard),
    ( 
        game_over(NewBoard, Winner),
        (Winner = 'b' -> write('Black is the winner !!!'), nl;
         Winner = 'w' -> write('White is the winner !!!'), nl),
         display_board(NewBoard)
    ;
        display_game(NewBoard, human, w)        % Continue the game if no winner
    ).
   


game_over(board(Rows), 'b') :-              %Game over function
    get_element(board(Rows), 8, 8, 'x'), !.

game_over(board(Rows), 'w') :-
    get_element(board(Rows), 1, 1, 'y').





read_move(Play, w, board(Rows)) :-
    write('Enter your move in the form ((X1,Y1),(X2,Y2)): '), nl,           %read white move
    read(Input),
    (   
        % Validate simple move
        Input = ((X1, Y1), (X2, Y2)),
        X1 >= 1, X1 =< 8,
        X2 >= 1, X2 =< 8,
        Y1 >= 1, Y1 =< 8,
        Y2 >= 1, Y2 =< 8,
        (Y2 - Y1 =:= -1; Y2 - Y1 =:= 0),
        (X2 - X1 =:= -1; X2 - X1 =:= 0)
    ->  
        Play = Input, 
        write('Valid move!'), nl
    ;   
        % Check jump validity
        check_jump(board(Rows), w, Input, Value),
        (Value = 'True' ->
            write('Valid jump!'), nl, Play = Input
        ;   
            write('Invalid play. Try a different one: '), nl,
            read_move(Play, w, board(Rows)))
    ).


read_move(Play, b,board(Rows)) :-
    write('Enter your move in the form ((X1,Y1),(X2,Y2)): '), nl,           %read black move  
    read(Input),
    (   % Check validity
        Input = ((X1, Y1), (X2, Y2)),
        X1 >= 1, X1 =< 8,
        X2 >= 1, X2 =< 8,
        Y1 >= 1, Y1 =< 8,
        Y2 >= 1, Y2 =< 8,
        (Y2 - Y1 =:= 1; Y2 - Y1 =:= 0),
        (X2 - X1 =:= 1; X2 - X1 =:= 0)
    ->  
        Play = Input, 
        write('Valid move!'), nl
    ;   
        % Check jump validity
        check_jump(board(Rows), b, Input, Value),
        (Value = 'True' ->
            write('Valid jump!'), nl, Play = Input
        ;   
            write('Invalid play. Try a different one: '), nl,
            read_move(Play, b, board(Rows)))
    ).

% Predicate to check jump validity
check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %horizontal white jump check
    X2 - X1 =:= 0,
    N is Y1 - Y2,
    N > 1,
    YNext is Y1 - 1,
    get_element(board(Rows), X1, YNext, Element),
    (Element = 'w'; Element = 'y'),
    check_jump(board(Rows), w, ((X1, YNext), (X2, Y2)), Value).

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %horizontal white jump check
    X2 - X1 =:= 0,
    N is Y1 - Y2,
    N =:= 1,
    get_element(board(Rows), X1, Y1 - 1, Element),
    (Element = '_'; Element = 'b'; Element = 'x'),
    Value = 'True'.



check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %vertical white jump check
    Y2 - Y1 =:= 0,
    N is X1 - X2,
    N > 1,
    XNext is X1 - 1,
    get_element(board(Rows), XNext, Y1, Element),
    (Element = 'w'; Element = 'y'),
    check_jump(board(Rows), w, ((XNext, Y1), (X2, Y2)), Value).

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %vertical white jump check
    Y2 - Y1 =:= 0,
    N is X1 - X2,
    N =:= 1,
    get_element(board(Rows), X1-1, Y1 , Element),
    (Element = '_'; Element = 'b'; Element = 'x'),
    Value = 'True'.


check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          % Diagonal white jump check 
    
    (
        DiffX is X2 - X1,
        DiffY is Y2 - Y1,
        DiffX =:= DiffY,        % Ensure diagonal path
        DiffX < 0,              % Moving in (-1, -1) direction
        diagonal_path(board(Rows), X1, Y1, X2, Y2)
    )
    -> Value = 'True'.



check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-              %horizontal black jump check
    % Recursive horizontal black jump check
    X1 - X2 =:= 0,
    N is Y2 - Y1,
    N > 1,
    YNext is Y1 + 1,
    get_element(board(Rows), X1, YNext, Element),
    (Element = 'b'; Element = 'x'), % Ensure valid intermediate pieces
    check_jump(board(Rows), b, ((X1, YNext), (X2, Y2)), Value).

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-              %horizontal black jump check
    % Final destination check for horizontal black jump
    X2 - X1 =:= 0,
    N is Y2 - Y1,
    N =:= 1,
    get_element(board(Rows), X1, Y2, Element),
    (Element = '_'; Element = 'w'; Element = 'y'), 
    Value = 'True'.



check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-              %vertical black jump check
    % Recursive vertical black jump check
    Y2 - Y1 =:= 0,
    N is X2 - X1,
    N > 1,
    XNext is X1 + 1,
    get_element(board(Rows), XNext, Y1, Element),
    (Element = 'b'; Element = 'x'), % Ensure valid intermediate pieces
    check_jump(board(Rows), b, ((XNext, Y1), (X2, Y2)), Value).

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-               %vertical black jump check
    % Final destination check for vertical black jump
    Y2 - Y1 =:= 0,
    N is X2 - X1,
    N =:= 1,
    get_element(board(Rows), X2, Y1, Element), % Fix: Check the target position
    (Element = '_'; Element = 'w'; Element = 'y'), % Fix: Ensure valid destination for black
    Value = 'True'.



check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-          %Diagonal black jump check 
    
    (
        DiffX is X2 - X1,
        DiffY is Y2 - Y1,
        DiffX =:= DiffY,        % Ensure diagonal path
        DiffX > 0,              % Moving in (1, 1) direction
        diagonal_path(board(Rows), X1, Y1, X2, Y2)
    )
    -> Value = 'True'.
    



check_jump(_, _, _, 'False').



diagonal_path(board(Rows), X1, Y1, X2, Y2) :-           %Recursive helper for black diagonal 
    % Base case: Reached the destination
    X1 =:= X2,
    Y1 =:= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = '_'; Element = 'w'; Element = 'y'). 

diagonal_path(board(Rows), X1, Y1, X2, Y2) :-
    % Recursive step: Check current position and move diagonally
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'b'; Element = 'x'), % Valid intermediate pieces
    XNext is X1 + 1,
    YNext is Y1 + 1,
    diagonal_path(board(Rows), XNext, YNext, X2, Y2).




diagonal_path(board(Rows), X1, Y1, X2, Y2) :-           %Recursive helper for white diagonal 
    % Base case: Reached the destination
    X1 =:= X2,
    Y1 =:= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = '_'; Element = 'b'; Element = 'x').

diagonal_path(board(Rows), X1, Y1, X2, Y2) :-
    % Recursive step: Check current position and move diagonally
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'w'; Element = 'y'), % Valid elements
    XNext is X1 - 1,
    YNext is Y1 - 1,
    diagonal_path(board(Rows), XNext, YNext, X2, Y2).







move(board(Rows),b,((X1,Y1),(X2,Y2)),NewBoard) :-           %move black
    get_element(board(Rows),X2,Y2,'b').



move(board(Rows),b,((X1,Y1),(X2,Y2)),NewBoard) :-           %move black
    get_element(board(Rows),X1,Y1,'b'),
    replace_element(board(Rows),X1,Y1,'_',NewBoard1),
    replace_element(NewBoard1,X2,Y2,'b',NewBoard);
    get_element(board(Rows),X1,Y1,'x'),
    replace_element(board(Rows),X1,Y1,'_',NewBoard1),
    replace_element(NewBoard1,X2,Y2,'x',NewBoard).






move(board(Rows),w,((X1,Y1),(X2,Y2)),NewBoard) :-           %move white
    get_element(board(Rows),X2,Y2,'w').



move(board(Rows),w,((X1,Y1),(X2,Y2)),NewBoard) :-           %move white
    get_element(board(Rows),X1,Y1,'w'),
    replace_element(board(Rows),X1,Y1,'_',NewBoard1),
    replace_element(NewBoard1,X2,Y2,'w',NewBoard);
    get_element(board(Rows),X1,Y1,'y'),
    replace_element(board(Rows),X1,Y1,'_',NewBoard1),
    replace_element(NewBoard1,X2,Y2,'y',NewBoard).









% display_board/1: Takes a board and prints it row by row
display_board(board(Rows)) :-
    write('  1 2 3 4 5 6 7 8'), nl, % Column headers
    display_rows(Rows, 1).         % Start with row number 1

% display_rows/2: Helper predicate to iterate over rows with a row index
display_rows([], _).
display_rows([Row|Rest], Index) :-
    write(Index), write(' '),       % Print row index
    display_row(Row),               % Print the row contents
    nl,                             % New line after each row
    NextIndex is Index + 1,         % Increment the row index
    display_rows(Rest, NextIndex).  % Recursive call for the remaining rows

% display_row/1: Helper predicate to print elements in a single row
display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),             % Print the cell
    write(' '),                     % Add a space after each cell
    display_row(Rest).              % Recursive call for the remaining cells

 
% display_cell/1: Helper predicate to display board elements
display_cell(Cell) :-
    (   var(Cell)                   % If the cell is uninstantiated
    ->  write('_')                  % Display as '_'
    ;   write(Cell)                 
    ).










replace_element(board(Rows), Row, Col, NewValue, board(NewRows)) :-
    nth1(Row, Rows, RowList),            % Get the Row-th row
    replace_in_row(Col, RowList, NewValue, NewRow), % Replace element in the row
    replace_in_board(Row, Rows, NewRow, NewRows).  % Replace modified row in the board

% Helper predicate to replace an element in a row
replace_in_row(1, [_|T], NewValue, [NewValue|T]).  % Replace first element in the row
replace_in_row(Col, [H|T], NewValue, [H|NewRow]) :-
    Col > 1,
    Col1 is Col - 1,
    replace_in_row(Col1, T, NewValue, NewRow).

% Helper predicate to replace a row in the board
replace_in_board(1, [_|T], NewRow, [NewRow|T]).  % Replace first row in the board
replace_in_board(Row, [H|T], NewRow, [H|NewBoard]) :-
    Row > 1,
    Row1 is Row - 1,
    replace_in_board(Row1, T, NewRow, NewBoard).


nth1(1, [H|_], H).
nth1(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth1(N1, T, X).

get_element(board(Rows), Row, Col, Element) :-
    nth1(Row, Rows, RowList),      % Get the Row-th row (1-based index)
    nth1(Col, RowList, Element).   % Get the Col-th element from that row




/*
% Directions for black and white players
direction(b, 1, 1).   % Black: Forward (down-right in the matrix)
direction(b, 1, 0).   % Black: Diagonal left (down in the matrix)
direction(b, 0, 1).   % Black: Diagonal right (right in the matrix)

direction(w, -1, -1). % White: Forward (up-left in the matrix)
direction(w, -1, 0).  % White: Diagonal left (up in the matrix)
direction(w, 0, -1).  % White: Diagonal right (left in the matrix)

*/


