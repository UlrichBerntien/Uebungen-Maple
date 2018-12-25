
Sudoku := module ()
    description "Sudoku solver";
    global `type/grid`;
    export string2grid, printGrid, solver;
    local newGrid, blockIndices, block, group;
    local insert, isAny, isOpen, isSolved, minimalCandidates;
    local trialAndErrorSolver, nakedSinglesSolver, hiddenSinglesSolver;
    option package;

    #
    #   A Sudoku grid.
    #   field : a field of the grid. Number 0 indicates an empty field.
    #   candiates : set of numbers, candiadates for the field.
    #
    `type/grid` := 'record( field ::Array, candidates ::Array )';


    printGrid := proc ( x ::grid )
        description "prints Sudoku grid to console";
        local rowIndex ::posint, colIndex ::posint;
        for rowIndex from 1 to 9 do
            for colIndex from 1 to 9 do
                if x:-field[rowIndex,colIndex] > 0 then
                    printf( "%1d", x:-field[rowIndex,colIndex] );
                else
                    printf( "." );
                end if;
            end do;
            printf( "\n" );
        end do;
    end proc;


    newGrid := proc () ::grid;
        description "creates an empty Sudoku grid";
        Record( field = Array(1..9,1..9,fill=0,datatype=integer),
                candidates = Array(1..9,1..9,fill={seq(i,i=1..9)},datatype=set(integer) ) );
    end proc;


    #
    #   Indices of the fields in a block.
    #
    blockIndices := Array( 0..2, 0..2, (row,col)->[seq(seq( [i+row*3,j+col*3] ,j=1..3),i=1..3)] );


    block := proc ( row ::posint, col ::posint ) ::list;
        description "gets all indices of a block";
        ASSERT( 0 < row and row < 10 and 0 < col and col < 10 );
        blockIndices[trunc((row-1)/3),trunc((col-1)/3)];
    end proc;


    group := proc ( i ::posint ) ::list;
        option remember;
        description "1..9 the columns, 10..18 the rows, 19..27 the blocks";
        if 1 <= i and i <= 9 then
            [ [j,i] $ j=1..9 ];
        elif 10 <= i and i <= 18 then
            [ [i-9,j] $ j=1..9 ];
        elif 19 <= i and i <= 27 then
            blockIndices[ trunc((i-19)/3), (i-19) mod 3];
        else
            error "index out of range"
        end if;
    end proc;
    
    
    insert := proc ( x ::grid, row ::posint, col ::posint, value ::posint )
        description "sets a field in the Sudoku grid, updates the candidates";
        local index ::posint;
        ASSERT( 1 <= value and value <= 9 );
        ASSERT( value in x:-candidates[row,col] and x:-field[row,col] = 0 );
        x:-field[row,col] := value;
        x:-candidates[row,col] := {};
        for index from 1 to 9 do
            x:-candidates[row,index] := x:-candidates[row,index] minus {value};
            x:-candidates[index,col] := x:-candidates[index,col] minus {value};
        end do;
        for index in block(row,col) do
            x:-candidates[op(index)] := x:-candidates[op(index)] minus {value};
        end do;
    end proc;
    
    
    string2grid := proc ( str ) ::grid;
        description "converts a single string or a iterable of strings to a Sudoku grid";
        local result ::grid, rowIndex ::posint, colIndex ::posint, values, v;
        if nops(str) = 1 then
            if length(str) <> 9*9 then
                error "invalid number of characters in string"
            end if;
            result := newGrid();
            values := convert( str, bytes ) -~ convert("0",bytes)[1];
            for rowIndex from 1 to 9 do
                for colIndex from 1 to 9 do
                    v := values[rowIndex*9+colIndex-9];
                    if  1 <= v and v <= 9 then
                        insert( result, rowIndex, colIndex, v );
                    end if;
                end do;
            end do;
        elif nops(str) = 9 then
            result := thisproc( cat(str) );
        else
            error "invalid number of strings"
        end if;
        result;
    end proc;


    isAny := proc ( x ::grid, condition )
        description "checks if a element exists which fulfills the condition";
        local rowIndex ::posint, colIndex ::posint;
        for rowIndex from 1 to 9 do
            for colIndex from 1 to 9 do
                if condition( x:-field[rowIndex,colIndex], x:-candidates[rowIndex,colIndex] ) then
                    return true;
                end if;
            end do;
        end do;
        false;
    end proc;


    isSolved := proc ( x ::grid ) ::boolean;
        description "checks if all fields are solved";
        not isAny( x, (field,candidates) -> field < 1 );
    end proc;


    isOpen := proc ( x ::grid ) ::boolean;
        description "checks if open fields are in the grid";
        not isAny( x, (field,candidates) -> field = 0 and candidates = {}  ) and isAny( x, (field,candidates) -> field = 0 );
    end proc;


    minimalCandidates := proc( x ::grid )
        description "found index (row,col) of minimal candidates set";
        local index, count, rowIndex ::posInt, colIndex ::posInt;
        count := 10;
        for rowIndex from 1 to 9 do
            for colIndex from 1 to 9 do
                if x:-field[rowIndex,colIndex] = 0 and nops(x:-candidates[rowIndex,colIndex]) < count then
                    count := nops(x:-candidates[rowIndex,colIndex]);
                    index := rowIndex, colIndex;
                end if;
            end do;
        end do;
        ASSERT( count > 0 );
        ASSERT( index <> 'index' );
        index;
    end proc;


    trialAndErrorSolver := proc ( x ::grid ) ::boolean;
        description "solves by trial and error";
        local index, v ::posint, backup;
        ASSERT( isOpen(x) and not isSolved(x) );
        index := minimalCandidates(x);
        backup := Record( field=copy(x:-field), candidates=copy(x:-candidates) );
        for v in x:-candidates(index) do
            insert( x, index, v );
            if solver( x ) then
                return true;
            else
                x:-field := copy(backup:-field);
                x:-candidates := copy(backup:-candidates);
            end if;
        end do;
        # All candidates failed
        false;
    end proc;


    nakedSinglesSolver := proc ( x ::grid ) ::integer;
        description "inserts the naked singles";
        local count ::integer, rowIndex ::posInt, colIndex ::posInt;
        # Count number of solved fields.
        count := 0;
        for rowIndex from 1 to 9 do
            for colIndex from 1 to 9 do
                if nops(x:-candidates[rowIndex,colIndex]) = 1 then
                    # Found a naked single. Only one candidate.
                    insert( x, rowIndex, colIndex, x:-candidates[rowIndex,colIndex][1] );
                    count := count + 1;
                end if;
            end do;
        end do;
        count;
    end proc;


    hiddenSinglesSolver := proc ( x ::grid ) ::integer;
        description "inserts the hidden singles";
        local count ::integer, groupIndex ::posInt, g ::list, c:: Array, index ::list, candidate ::posInt;
        # Count number of solved fields
        count := 0;
        # Search in all groups for hidden singles
        for groupIndex from 1 to 3*9 do
            g := group(groupIndex);
            c := Array(1..9,fill=0);
            for index in g do
                # Count all candidates.
                for candidate in x:-candidates[op(index)] do
                    c[candidate] := c[candidate] + 1;
                end do;
            end do;
            for candidate from 1 to 9 do
                if c[candidate] = 1 then
                    count := count + 1;
                    # This candidate is a hidden single.
                    for index in g do
                        if candidate in x:-candidates[op(index)] then
                            # insert the hidden single.
                            insert( x, op(index), candidate );
                        end if;
                    end do;
                end if;
            end do;
        end do;
        count;
    end proc;


    solver := proc ( x ::grid ) ::boolean;
        description "solves sudokus, finds one solution";
        local changes ::integer;
        changes := 1;
        # Use the algorithmic solver until there are no open fields or the solver don't work
        while isOpen(x) and changes > 0 do
            changes :=   nakedSinglesSolver(x)
                       + hiddenSinglesSolver(x);
        end do;
        if isOpen(x) then
            # Last chance. Solve by trial and error (backtracking algorithm)
            trialAndErrorSolver(x);
        end if;
        isSolved(x);
    end proc;


end module;
