:- use_module(library(random)).

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
    ]),Player1,Player2,w).
    

    board([
        ['x', 'b', 'b', 'b', '_', '_','_', '_'],
        ['b', 'b', 'b', 'b', '_', '_', '_', '_'],
        ['b', 'b', '_', '_','_', '_', '_', '_'],
        ['b', 'b', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', 'w', 'w'],
        ['_', '_', '_', '_', '_', '_', 'w', 'w'],
        ['_', '_', '_', '_',  'w', 'w', 'w', 'w'],
        ['_', '_', '_', '_',  'w', 'w', 'w', 'y']
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%DISPLAY%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_game(board(Rows), computer,Player2,w) :-            %Display game for computer
    write('Computer (white) is gonna play'), nl,
    display_board(board(Rows)), nl,
    choose_move(computer,Play, w, board(Rows)),
    move(board(Rows), w, Play, NewBoard),
    ( 
        (game_over(board(Rows), Play, Winner) ; game_over(NewBoard, Winner)), %check if game is over
        (Winner = 'b' -> write('Black is the winner !!!'), nl;
         Winner = 'w' -> write('White is the winner !!!'), nl),
        display_board(NewBoard),play
    ;
        display_game(NewBoard, Player2,computer, b)        %Continue the game if no winner
    ).

display_game(board(Rows), computer,Player2,b) :-            %Display game for white, computer
    write('Computer (black) is gonna play'), nl,
    display_board(board(Rows)), nl,
    choose_move(computer,Play, b, board(Rows)),
    move(board(Rows), b, Play, NewBoard),
    ( 
        (game_over(board(Rows), Play, Winner) ; game_over(NewBoard, Winner)),       %check if game is over
        (Winner = 'b' -> write('Black is the winner !!!'), nl;
         Winner = 'w' -> write('White is the winner !!!'), nl),
        display_board(NewBoard),play
    ;
        display_game(NewBoard, Player2,computer, w)        %Continue the game if no winner
    ).


display_game(board(Rows), human, Player2, w) :-           %Display game for white, human
    write('White to play'), nl,
    display_board(board(Rows)), nl,
    valid_moves(board(Rows),w,Moves),
    display_moves(Moves),
    write('Insert your play: '), nl,
    choose_move(Play, w, board(Rows)),
    write('Good move!'), nl,
    move(board(Rows), w, Play, NewBoard),
    ( 
        (game_over(board(Rows), Play, Winner) ; game_over(NewBoard, Winner)),
        (Winner = 'b' -> write('Black is the winner !!!'), nl;
         Winner = 'w' -> write('White is the winner !!!'), nl),
        display_board(NewBoard),play
    ;
        display_game(NewBoard, Player2,human, b)        % Continue the game if no winner
    ).

display_game(board(Rows), human,Player2,b) :-        %display game for black, human
    write('Black to play'), nl,
    display_board(board(Rows)), nl,
    valid_moves(board(Rows),b,Moves),
    display_moves(Moves),
    write('Insert your play: '),nl,
    choose_move(Play,b,board(Rows)),
    write('good move!'),nl,
    move(board(Rows),b,Play,NewBoard),
    (   
        (game_over(board(Rows), Play, Winner) ; game_over(NewBoard, Winner)),
        (Winner = 'b' -> write('Black is the winner !!!'), nl;
         Winner = 'w' -> write('White is the winner !!!'), nl),
        display_board(NewBoard),play  
    ;   
        display_game(NewBoard, Player2,human, w)        % Continue the game if no winner
    ).
   

   display_moves([]) :- write('No more valid moves available.'), nl.
display_moves([((X1, Y1), (X2, Y2)) | Rest]) :-
    format('From (~w, ~w) to (~w, ~w). ', [X1, Y1, X2, Y2]), 
    display_moves(Rest).


display_board(board(Rows)) :-           %function to display the board in the correct format
       
    length(Rows, RowCount),          
    display_rows_from_bottom(Rows, RowCount),
    write('  1 2 3 4 5 6 7 8'). 


display_rows_from_bottom(_, 0).       %helper for displaying each row
display_rows_from_bottom(Rows, Index) :-
    nth1(Index, Rows, Row),           
    write(Index), write(' '),         
    display_row(Row),                 
    nl,                               
    PrevIndex is Index - 1,           
    display_rows_from_bottom(Rows, PrevIndex). 


display_row([]).                        %helper for displaying each element in a row
display_row([Cell|Rest]) :-
    display_cell(Cell),               
    write(' '),                      
    display_row(Rest).               


display_cell(Cell) :-                   %helper to display empty cells
    (   var(Cell)                     
    ->  write('_')                   
    ;   write(Cell)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%GAMEOVER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game_over(board(Rows),((X1,Y1),(X2,Y2)),'b'):-      %Game over if king is eaten
    get_element(board(Rows),X2,Y2,'y').

game_over(board(Rows),((X1,Y1),(X2,Y2)),'w'):-
    get_element(board(Rows),X2,Y2,'x').

game_over(board(Rows), 'b') :-                      %Game over if king is on the enemy corner
    get_element(board(Rows), 8, 8, 'x'), !.

game_over(board(Rows), 'w') :-
    get_element(board(Rows), 1, 1, 'y').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%MOVE_CHECKS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_transform((X1, Y1), w, board(Rows), Value) :-                         %check transform white
    get_element(board(Rows), X1, Y1, Element),
    (   Element \= 'w'                                                     
    ->  Value = 'False', !                                                
    ;                                 
    (   search_direction((X1, Y1),  0,  1, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1),  0, -1, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1),  1,  0, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1), -1,  0, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1),  1,  1, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1),  1, -1, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1), -1,  1, Rows, 'y') -> Value = 'True' 
    ;   search_direction((X1, Y1), -1, -1, Rows, 'y') -> Value = 'True' 
    ;   Value = 'False' 
    )
    ).

check_transform((X1, Y1), b, board(Rows), Value) :-                         %Check transform black
    get_element(board(Rows), X1, Y1, Element),
    (   Element \= 'b'                                                     
    ->  Value = 'False', !                                                
    ;   (   search_directionb((X1, Y1),  0,  1, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1),  0, -1, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1),  1,  0, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1), -1,  0, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1),  1,  1, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1),  1, -1, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1), -1,  1, Rows, 'x') -> Value = 'True' 
        ;   search_directionb((X1, Y1), -1, -1, Rows, 'x') -> Value = 'True' 
        ;   Value = 'False'                                                
        )
    ).

search_direction((X, Y), DX, DY, Rows, Target) :-                   %helper for checking line of sight in a direction for white
    NX is X + DX,
    NY is Y + DY,
    within_bounds(NX, NY),
    get_element(board(Rows), NX, NY, Element),
    (   Element = Target  % Found target
    ;   (Element \= 'b', Element \= 'x', search_direction((NX, NY), DX, DY, Rows, Target)) 
    ), !.  

search_directionb((X, Y), DX, DY, Rows, Target) :-                  %helper for checking line of sight in a direction for black
    NX is X + DX,
    NY is Y + DY,
    within_bounds(NX, NY),              
    get_element(board(Rows), NX, NY, Element),
    (   Element = Target               
    ;   Element \= 'w', Element \= 'y', 
        search_directionb((NX, NY), DX, DY, Rows, Target) 
    ).

within_bounds(X, Y) :-            %helper for checking if coordinates are inside the board
    X > 0, X =< 8,
    Y > 0, Y =< 8.

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %horizontal white jump check
    X2 - X1 =:= 0,
    N is Y1 - Y2,
    N > 1,
    YNext is Y1 - 1,
    get_element(board(Rows), X1, YNext, Element2),
    (Element2 = 'w'; Element2 = 'y'),
    check_jump(board(Rows), w, ((X1, YNext), (X2, Y2)), Value).

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %horizontal white jump check base case
    X2 - X1 =:= 0,
    N is Y1 - Y2,
    N =:= 1,
    Value = 'True'.

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %vertical white jump check
    Y2 - Y1 =:= 0,
    N is X1 - X2,
    N > 1,
    XNext is X1 - 1,
    get_element(board(Rows), XNext, Y1, Element2),
    (Element2 = 'w'; Element2 = 'y'),
    check_jump(board(Rows), w, ((XNext, Y1), (X2, Y2)), Value).

check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-          %vertical white jump check base case
    Y2 - Y1 =:= 0,
    N is X1 - X2,
    N =:= 1,
    Value = 'True'.


check_jump(board(Rows), w, ((X1, Y1), (X2, Y2)), Value) :-         %white diagonal jump
    DiffX is X2 - X1,
    DiffY is Y2 - Y1,
    DiffX =:= DiffY,        
    DiffX < 0,              
    (
        diagonal_path(board(Rows), X1, Y1, X2, Y2) 
        -> Value = 'True'; 
        Value = 'False'     
    ).

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-         %black diagonal jump 
    DiffX is X2 - X1,
    DiffY is Y2 - Y1,
    DiffX =:= DiffY,        
    DiffX > 0,              
    (
        diagonal_pathb(board(Rows), X1, Y1, X2, Y2) 
        -> Value = 'True'; 
        (
            
            Value = 'False' )    
    ).

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-              %horizontal black jump check base 
    X1 - X2 =:= 0,
    N is Y2 - Y1,
    N > 1,
    YNext is Y1 + 1,
    get_element(board(Rows), X1, YNext, Element),
    (Element = 'b'; Element = 'x'), % Ensure valid intermediate pieces
    check_jump(board(Rows), b, ((X1, YNext), (X2, Y2)), Value).

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-              %horizontal black jump check base case
    % Final destination check for horizontal black jump
    X2 - X1 =:= 0,
    N is Y2 - Y1,
    N =:= 1,
    get_element(board(Rows), X2, Y2, Element),
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

check_jump(board(Rows), b, ((X1, Y1), (X2, Y2)), Value) :-               %vertical black jump check base case
    % Final destination check for vertical black jump
    Y2 - Y1 =:= 0,
    N is X2 - X1,
    N =:= 1,
    get_element(board(Rows), X2, Y2, Element), 
    (Element = '_'; Element = 'w'; Element = 'y'), 
    Value = 'True'.

check_jump(_, _, _, 'False').                           %check jump base case

diagonal_path(board(Rows), X1, Y1, X2, Y2) :-           %helper for white diagonal jump base case
    X1 =:= X2,
    Y1 =:= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = '_'; Element = 'b'; Element = 'x').  % Valid destination for white

diagonal_path(board(Rows), X1, Y1, X2, Y2) :-           %helper for white diagonal jump
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'w'; Element = 'y'),  
    XNext is X1 - 1,
    YNext is Y1 - 1,
    diagonal_path(board(Rows), XNext, YNext, X2, Y2).

diagonal_path(board(Rows), X1, Y1, X2, Y2) :-           %helper to identify diagonals with enemy pieces
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    \+ (Element = 'w'; Element = 'y'),  
    !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%white black diagonals%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diagonal_pathb(board(Rows), X1, Y1, X2, Y2) :-       %helper for black diagonal jump base case    
    X1 =:= X2,
    Y1 =:= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = '_'; Element = 'w'; Element = 'y').  %Valid destination for black

diagonal_pathb(board(Rows), X1, Y1, X2, Y2) :-              %helper for black diagonal jump
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'b'; Element = 'x'),  
    XNext is X1 + 1,
    YNext is Y1 + 1,
    diagonal_pathb(board(Rows), XNext, YNext, X2, Y2).


diagonal_pathb(board(Rows), X1, Y1, X2, Y2) :-          %helper to identify diagonals with enemy pieces
    X1 \= X2,
    Y1 \= Y2,
    get_element(board(Rows), X1, Y1, Element),
    \+ (Element = 'b'; Element = 'x'),  
    !, fail.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%VALID_MOVES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_moves(board(Rows), b, Moves) :-
    findall(
        ((X1, Y1), (X2, Y2)), 
        (
            between(1, 8, X1),
            between(1, 8, Y1),
            get_element(board(Rows),X1,Y1,Element),
            (Element = 'x'; Element = 'b'),
            between(1, 8, X2),
            between(1, 8, Y2),
            validate_move(((X1, Y1), (X2, Y2)), b, board(Rows), Valid, _),
            Valid = 'True'
        ), 
        RawMoves
    ),    
    sort(RawMoves, Moves),      %Remove duplicates 
    length(Moves, Count),
    write('Number of valid moves: '), write(Count), nl.

valid_moves(board(Rows), w, Moves) :-
    findall(
        ((X1, Y1), (X2, Y2)), 
        (   
            between(1, 8, X1),
            between(1, 8, Y1),
            get_element(board(Rows),X1,Y1,Element),
            (Element = 'y'; Element = 'w'),
            between(1, 8, X2),
            between(1, 8, Y2),
            validate_move(((X1, Y1), (X2, Y2)), w, board(Rows), Valid, ((X1, Y1), (X2, Y2))),
            Valid = 'True'
        ), 
        RawMoves
    ),    
    sort(RawMoves, Moves),          %Remove duplicates 
    length(Moves, Count),
    write('Number of valid moves: '), write(Count), nl.    

between(Low, High, Low) :-
    integer(Low),
    integer(High),
    Low =< High.

between(Low, High, Value) :-
    integer(Low),
    integer(High),
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%CHOOSE_MOVE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % %  must have functionality to distinguish between random and greedy  % % %

choose_move(computer,Play, w, board(Rows)):-
%%  valid_moves(board(Rows),w,Moves),
%%  random_member(Play,Moves).                       % random
    best_move(board(Rows), w, Play).                 % greedy

choose_move(computer,Play, b, board(Rows)):-
%%  valid_moves(board(Rows),b,Moves),
%%  random_member(Play,Moves).                       % random
    best_move(board(Rows), b, Play).                 % greedy

choose_move(Play, w, board(Rows)) :-
    write('Enter your move in the form ((X1,Y1),(X2,Y2)): '), nl,
    catch(
        (read_safe(Input), validate_and_handle_input(Input, Play, w, board(Rows))),
        invalid_input,
        (write('Invalid input format. Try again.'), nl, choose_move(Play, w, board(Rows)))
    ).

choose_move(Play, b, board(Rows)) :-
    write('Enter your move in the form ((X1,Y1),(X2,Y2)): '), nl,
    catch(
        (read_safe(Input), validate_and_handle_input(Input, Play, b, board(Rows))),
        invalid_input,
        (write('Invalid input format. Try again.'), nl, choose_move(Play, b, board(Rows)))
    ).

read_safe(Input) :-
    catch(
        (read(Input), validate_input(Input)),  
        _Error,
        (write('Invalid input. Try again.'), nl, read_safe(Input))  %Catch syntax errors
    ).

validate_input(((X1, Y1), (X2, Y2))) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2),  
    between(1, 8, X1), between(1, 8, Y1),               
    between(1, 8, X2), between(1, 8, Y2), !.            

validate_input(_) :-
    throw(invalid_input).  %Throw an exception for invalid terms

validate_and_handle_input(Input, Play, w, board(Rows)) :-
    validate_move(Input, w, board(Rows), Valid, Play),
    (Valid = 'True' ->
        true
    ;
        write('Invalid move. Try again.'), nl,
        choose_move(Play, w, board(Rows))
    ).

validate_and_handle_input(Input, Play, b, board(Rows)) :-
    validate_move(Input, b, board(Rows), Valid, Play),
    (Valid = 'True' ->
        true
    ;
        write('Invalid move. Try again.'), nl,
        choose_move(Play, b, board(Rows))
    ).

validate_move(Input, w, board(Rows), Valid, Play) :-            %validate a white move
    Input = ((X1, Y1), (X2, Y2)),
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'w'; Element = 'y'),
    within_bounds(X1, Y1),
    within_bounds(X2, Y2),
    get_element(board(Rows), X2, Y2, Element1),

    (
        Element = 'w',                  %Transform move
        X1 =:= X2, Y1 =:= Y2,
        check_transform((X1, Y1), w, board(Rows), Value),
        Value = 'True',
        %write('Valid transform!'), nl,
        Play = Input, Valid = 'True',!
    ;           
        (Element1 = '_';Element1='b';Element1='x'), %Simple move
        validate_simple_move_w(X1, Y1, X2, Y2),
        %write('Valid move!'), nl,
        Play = Input, Valid = 'True',!
    ;          
        (Element1 = '_';Element1='b';Element1='x'), %Jump move
        check_jump(board(Rows), w, Input, Value),
        Value = 'True',
        %write('Valid jump!'), nl,
        Play = Input, Valid = 'True',!
    ;
        Valid = 'False'             %Invalid move
    ).


validate_move(Input, b, board(Rows), Valid, Play) :-        %validate a black move
    Input = ((X1, Y1), (X2, Y2)),
    get_element(board(Rows), X1, Y1, Element),
    (Element = 'b'; Element = 'x'),
    within_bounds(X1, Y1),
    within_bounds(X2, Y2),
    get_element(board(Rows), X2, Y2, Element1),

    (        
        Element = 'b',          %Transform move
        X1 =:= X2, Y1 =:= Y2,
        check_transform((X1, Y1), b, board(Rows), Value),
        Value = 'True',
        %write('Valid transform!'), nl,
        Play = Input, Valid = 'True',!
    ;
        (Element1 = '_';Element1='w';Element1='y'), %Simple move
        validate_simple_move(X1, Y1, X2, Y2),
        %write('Valid move!'), nl,
        Play = Input, Valid = 'True',!
    ;
        
        (Element1 = '_';Element1='w';Element1='y'), %Jump move
        check_jump(board(Rows), b, Input, Value),
        Value = 'True',
        %write('Valid jump!'), nl,
        Play = Input, Valid = 'True',!
    ;
        Valid = 'False'          %Invalid move
    ).

validate_simple_move_w(X1, Y1, X2, Y2) :-
    (Y2 - Y1 =:= -1, X2 - X1 =:= -1);
    (X2 - X1 =:= -1, Y2 - Y1 =:= 0);
    (Y2 - Y1 =:= 0, X2 - X1 =:= -1).

validate_simple_move(X1, Y1, X2, Y2) :-
    (Y2 - Y1 =:= 1, X2 - X1 =:= 1);
    (X2 - X1 =:= 1, Y2 - Y1 =:= 0);
    (Y2 - Y1 =:= 0, X2 - X1 =:= 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%MOVE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(board(Rows),w,((X1,Y1),(X2,Y2)),NewBoard):-            %transform white
    X1=:=X2,
    Y1=:=Y2,
    replace_element(board(Rows),X1,Y1,'y',NewBoard).

move(board(Rows),b,((X1,Y1),(X2,Y2)),NewBoard):-            %transform black
    X1=:=X2,
    Y1=:=Y2,
    replace_element(board(Rows),X1,Y1,'x',NewBoard).

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

replace_element(board(Rows), Row, Col, NewValue, board(NewRows)) :-
    nth1(Row, Rows, RowList),            
    replace_in_row(Col, RowList, NewValue, NewRow), 
    replace_in_board(Row, Rows, NewRow, NewRows).  


replace_in_row(1, [_|T], NewValue, [NewValue|T]).  
replace_in_row(Col, [H|T], NewValue, [H|NewRow]) :-
    Col > 1,
    Col1 is Col - 1,
    replace_in_row(Col1, T, NewValue, NewRow).


replace_in_board(1, [_|T], NewRow, [NewRow|T]). 
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
    nth1(Row, Rows, RowList),      
    nth1(Col, RowList, Element).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%VALUE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

value(board(Rows), Player, Value) :-
    catch(
        (
            opponent(Player, Opponent),

            % Get player's pieces
            get_player_non_kings(board(Rows), Player, PlayerNonKings),
            get_player_kings(board(Rows), Player, PlayerKings),

            % Get opponent's pieces
            get_opponent_non_kings(board(Rows), Player, OpponentNonKings),
            get_opponent_kings(board(Rows), Player, OpponentKings),

            % Calculate Material Count
            length(PlayerNonKings, NumPlayerNonKings),
            length(PlayerKings, NumPlayerKings),
            length(OpponentNonKings, NumOpponentNonKings),
            length(OpponentKings, NumOpponentKings),
            MaterialScore is (NumPlayerNonKings + 3 * NumPlayerKings) - 
                            (NumOpponentNonKings + 3 * NumOpponentKings),
            
            % Calculate Positional Advantage
            positional_advantage(board(Rows), Player, PlayerPos),
            positional_advantage(board(Rows), Opponent, OpponentPos),
            PositionalScore is PlayerPos - OpponentPos,

            % Calculate King Mobility
            king_mobility(board(Rows), Player, PlayerMobility),
            king_mobility(board(Rows), Opponent, OpponentMobility),
            MobilityScore is PlayerMobility - OpponentMobility,

            % Calculate Threats and Defenses
            threats_defenses(board(Rows), Player, PlayerThreats),
            threats_defenses(board(Rows), Opponent, OpponentThreats),
            ThreatsScore is PlayerThreats - OpponentThreats,

            % Calculate Big Score
            (
                % Player's king dies or opponent's king reaches player's corner
                (PlayerKings = [], BigScore = -999999);
                (player_corner(Player, CornerX, CornerY), get_element(board(Rows), CornerX, CornerY, OpponentKing), is_king(OpponentKing, Opponent), BigScore = -99999);
                
                % Opponent's king dies or player's king reaches opponent's corner
                (OpponentKings = [], BigScore = 999999);
                (player_corner(Opponent, OppCornerX, OppCornerY), get_element(board(Rows), OppCornerX, OppCornerY, PlayerKing), is_king(PlayerKing, Player), BigScore = 99999);
                
                % Default case
                BigScore = 0
            ),
            
            % Assign Weights
            MaterialWeight = 0.2,
            PositionalWeight = 0.3,
            MobilityWeight = 0.2,
            ThreatsWeight = 0.3,

            % Compute Total Value
            Value is (MaterialScore * MaterialWeight) +
                    (PositionalScore * PositionalWeight) +
                    (MobilityScore * MobilityWeight) +
                    (ThreatsScore * ThreatsWeight) + BigScore
        ),
        _Error,
        (Value = 0)
    ).

% Calculate positional advantage based on distance to opponent's corner
positional_advantage(board(Rows), Player, Score) :-
    player_corner(Player, CornerX, CornerY),
    findall(Distance, (
        nth1(X, Rows, Row),
        nth1(Y, Row, Cell),
        is_player_piece(Cell, Player),
        manhattan_distance(X, Y, CornerX, CornerY, Distance)
    ), Distances),
    sum_list(Distances, TotalDistance),
    Score is -TotalDistance.  % Lower distance is better

manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    Distance is DX + DY.

% Calculate king mobility
king_mobility(board(Rows), Player, Mobility) :-
    findall(Move, (
        nth1(X, Rows, Row),
        nth1(Y, Row, Cell),
        is_king(Cell, Player),
        (
            check_transform((X, Y), Player, board(Rows), 'True');
            check_jump(board(Rows), Player, ((X, Y), (X, Y)), 'True')
        )
    ), Moves),
    length(Moves, Mobility).

% Calculate threats and defenses
threats_defenses(board(Rows), Player, Score) :-
    findall(Threat, (
        nth1(X, Rows, Row),
        nth1(Y, Row, Cell),
        is_player_piece(Cell, Player),
        threatened(board(Rows), Player, (X, Y))
    ), Threats),
    length(Threats, Score).

% Determine if a piece is threatening
threatened(board(Rows), Player, (X, Y)) :-
    opponent(Player, Opponent),
    adjacent_positions(X, Y, Adjacent),
    member((AX, AY), Adjacent),
    within_bounds(AX, AY),
    nth1(AX, Rows, Row),
    nth1(AY, Row, OpponentCell),
    is_player_piece(OpponentCell, Opponent).

% Get adjacent positions
adjacent_positions(X, Y, Adjacent) :-
    DXList = [-1, 0, 1],
    DYList = [-1, 0, 1],
    findall((NX, NY), (
        member(DX, DXList),
        member(DY, DYList),
        (DX \= 0; DY \= 0),
        NX is X + DX,
        NY is Y + DY
    ), Adjacent).

% Check if a cell contains a player's piece (regular or king)
is_player_piece(Cell, Player) :-
    (Player = w, member(Cell, ['w', 'y']));
    (Player = b, member(Cell, ['b', 'x'])).

% Check if a cell contains a player's king piece
is_king(Cell, Player) :-
    (Player = w, Cell = 'y');
    (Player = b, Cell = 'x').

get_player_non_kings(board(Rows), w, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'w')), NonKings).

get_player_non_kings(board(Rows), b, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'b')), NonKings).

get_opponent_non_kings(board(Rows), w, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'b')), NonKings).

get_opponent_non_kings(board(Rows), b, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'w')), NonKings).

get_player_kings(board(Rows), w, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'y')), NonKings).

get_player_kings(board(Rows), b, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'x')), NonKings).

get_opponent_kings(board(Rows), w, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'x')), NonKings).

get_opponent_kings(board(Rows), b, NonKings) :-
    findall((X, Y), (between(1, 8, X), between(1, 8, Y), get_element(board(Rows), X, Y, 'y')), NonKings).

opponent(w, b).
opponent(b, w).

player_corner(w, 8, 8).
player_corner(b, 1, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%BEST MOVE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

best_move(board(Rows), Player, BestMove) :-
    valid_moves(board(Rows), Player, Moves),
    Moves \= [],
    best_move_recursive(Moves, board(Rows), Player, -99999, none, BestMove).

best_move_recursive([], _Board, _Player, _CurrentBestValue, BestMove, BestMove).
best_move_recursive([Move|Rest], board(Rows), Player, CurrentBestValue, CurrentBestMove, BestMove) :-
    move(board(Rows), Player, Move, NewBoard),
    value(NewBoard, Player, MoveValue),
    (
        MoveValue > CurrentBestValue
        -> NewBestValue = MoveValue,
           NewBestMove = Move
        ;  NewBestValue = CurrentBestValue,
           NewBestMove = CurrentBestMove
    ),
    best_move_recursive(Rest, board(Rows), Player, NewBestValue, NewBestMove, BestMove).
