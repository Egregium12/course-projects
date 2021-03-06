##
##=======================================================
## Yu Li 
## CS 116 Winter 2021
## Final Project
##=======================================================
##

import check
import copy  # copies nested list to avoid mutating the consumed lists


## A Board, B, is a (listof (listof (anyof Str Nat Guess))
## Requires:
##   len(B) > 0 and the length of each inner list equals len(B)
##   If B[i][j] is a Str, len(B[i][j]) == 1.
##     (i.e. each cage is represented by a string of length one)
##   If B[i][j] is a Nat, then it is between 1 and len(B) (inclusive).
##     (i.e. all filled in numbers are in the valid range)
##   If B[i][j] is a Guess, then
##     B[i][j].number is between 1 and len(B) (inclusive).
##     (i.e. all guessed numbers are in the valid range)

## A Constraint, C, is a (list Str Nat (anyof '+' '-' '*' '/' '='))
## Requires:
##   len(C[0]) == 1
##   C[1] > 0

class Puzzle:
    '''
      Fields:
         size (Nat)
         board (Board)
         constraints (listof Constraint)
         Requires:
            size > 0
            len(board) == size
            If board[i][j] is a Guess, board[i][j].symbol is a cage in the
            puzzle. There is a one to one correspondence between the cages in
            board and constraints. For example, if "a" represents a cage in the
            puzzle, it appears in board and there is exactly one constraint for
            "a". If constraints[i][2] is "=", then the cage constraints[i][0]
              appears exactly once in the puzzle.
            If constraints[i][2] is "\" or "-", then the cage constraints[i][0]
              appears exactly twice in the puzzle.
    '''

    def __init__(self, size, board, constraints):
        '''
        Initializes a Puzzle.

        Effects: Mutates self

        __init__: Puzzle Nat Board (listof Constraint) -> None
        Requires: size > 0
        '''
        self.size = size
        self.board = board
        self.constraints = constraints

    def __eq__(self, other):
        '''
        Returns True if self and other are equal. False otherwise.

        __eq__: Puzzle Any -> Bool
        '''
        return (isinstance(other, Puzzle)) and \
               self.size == other.size and \
               self.board == other.board and \
               self.constraints == other.constraints

    def __repr__(self):
        '''
        Returns a string representation of self.

        __repr__: Puzzle -> Str
        '''
        s = 'Puzzle(\nSize=' + str(self.size) + '\n' + "Board:\n"
        for i in range(self.size):
            for j in range(self.size):
                if isinstance(self.board[i][j], Guess):
                    s = s + str(self.board[i][j]) + ' '
                else:
                    s = s + str(self.board[i][j]) + ' ' * 12
            s = s + '\n'
        s = s + "Constraints:\n"
        for i in range(len(self.constraints)):
            s = s + '[ ' + self.constraints[i][0] + '  ' + \
                str(self.constraints[i][1]) + '  ' + self.constraints[i][2] + \
                ' ]' + '\n'
        s = s + ')'
        return s


class Guess:
    '''
    Fields:
       symbol (Str)
       number (Nat)
       Requires:
         len(symbol) == 1
    '''

    def __init__(self, symbol, number):
        '''
        Initializes a Guess.

        Effects: Mutates self

        __init__: Guess Str Nat -> None
        '''
        self.symbol = symbol
        self.number = number

    def __repr__(self):
        '''
        Returns a string representation of self.

        __repr__: Guess -> Str
        '''
        return "Guess('{0}',{1})".format(self.symbol, self.number)

    def __eq__(self, other):
        '''
        Returns True if self and other are equal. False otherwise.

        __eq__: Guess Any -> Bool
        '''
        return (isinstance(other, Guess)) and \
               self.symbol == other.symbol and \
               self.number == other.number


class Posn:
    '''
    Fields:
       x (Nat)
       y (Nat)
       Note: Origin (where x=0 and y=0) is top left.
    '''

    def __init__(self, x, y):
        '''
        Initializes a Posn.

        Effects: Mutates self

        __init__: Posn Nat Nat -> None
        '''
        self.x = x
        self.y = y

    def __repr__(self):
        '''
        Returns a string representation of self.

        __repr__: Posn -> Str
        '''
        return "Posn({0},{1})".format(self.x, self.y)

    def __eq__(self, other):
        '''
        Returns True if self and other are equal. False otherwise.

        __eq__: Posn Any -> Bool
        '''
        return (isinstance(other, Posn)) and \
               self.x == other.x and \
               self.y == other.y

    ## ******** TESTING VALUES ***************


## Note: These are also used in the examples below.

puzzle1 = Puzzle(4, [['a', 'b', 'b', 'c'],
                     ['a', 'd', 'e', 'e'],
                     ['f', 'd', 'g', 'g'],
                     ['f', 'h', 'i', 'i']],
                 [['a', 6, '*'],
                  ['b', 3, '-'],
                  ['c', 3, '='],
                  ['d', 5, '+'],
                  ['e', 3, '-'],
                  ['f', 3, '-'],
                  ['g', 2, '/'],
                  ['h', 4, '='],
                  ['i', 1, '-']])

puzzle1partial = Puzzle(4, [['a', 'b', 'b', 'c'],
                            ['a', 2, 1, 4],
                            ['f', 3, 'g', 'g'],
                            ['f', 'h', 'i', 'i']],
                        [['a', 6, '*'],
                         ['b', 3, '-'],
                         ['c', 3, '='],
                         ['f', 3, '-'],
                         ['g', 2, '/'],
                         ['h', 4, '='],
                         ['i', 1, '-']])

## a partial solution to puzzle1 with a cage partially filled in
puzzle1partial2 = Puzzle(4, [[Guess('a', 2), 'b', 'b', 'c'],
                             ['a', 2, 1, 4],
                             ['f', 3, 'g', 'g'],
                             ['f', 'h', 'i', 'i']],
                         [['a', 6, '*'],
                          ['b', 3, '-'],
                          ['c', 3, '='],
                          ['f', 3, '-'],
                          ['g', 2, '/'],
                          ['h', 4, '='],
                          ['i', 1, '-']])

## a partial solution to puzzle1 with a cage partially filled in
##  but not yet verified
puzzle1partial3 = Puzzle(4, [[Guess('a', 2), 'b', 'b', 'c'],
                             [Guess('a', 3), 2, 1, 4],
                             ['f', 3, 'g', 'g'],
                             ['f', 'h', 'i', 'i']],
                         [['a', 6, '*'],
                          ['b', 3, '-'],
                          ['c', 3, '='],
                          ['f', 3, '-'],
                          ['g', 2, '/'],
                          ['h', 4, '='],
                          ['i', 1, '-']])

## a partial solution to puzzle1 with a cage partially filled in
##  but not yet verified and incorrect guess
puzzle1partial4 = Puzzle(4, [[2, 'b', 'b', 'c'],
                             [3, 2, 1, 4],
                             ['f', 3, 'g', 'g'],
                             ['f', 'h', 'i', 'i']],
                         [['b', 3, '-'],
                          ['c', 3, '='],
                          ['f', 3, '-'],
                          ['g', 2, '/'],
                          ['h', 4, '='],
                          ['i', 1, '-']])

puzzle1partial4a = Puzzle(4, [[2, Guess('b', 1), Guess('b', 3), 'c'],
                              [3, 2, 1, 4],
                              ['f', 3, 'g', 'g'],
                              ['f', 'h', 'i', 'i']],
                          [['b', 3, '-'],
                           ['c', 3, '='],
                           ['f', 3, '-'],
                           ['g', 2, '/'],
                           ['h', 4, '='],
                           ['i', 1, '-']])

puzzle1partial4b = Puzzle(4, [[2, Guess('b', 1), Guess('b', 4), 'c'],
                              [3, 2, 1, 4],
                              ['f', 3, 'g', 'g'],
                              ['f', 'h', 'i', 'i']],
                          [['b', 3, '-'],
                           ['c', 3, '='],
                           ['f', 3, '-'],
                           ['g', 2, '/'],
                           ['h', 4, '='],
                           ['i', 1, '-']])

## The solution to puzzle 1
puzzle1soln = Puzzle(4,
                     [[2, 1, 4, 3], [3, 2, 1, 4], [4, 3, 2, 1], [1, 4, 3, 2]],
                     [])

## For part h

puzzle1_first_guess = [
    Puzzle(4, [[Guess('a', 1), 'b', 'b', 'c'],
               ['a', 'd', 'e', 'e'],
               ['f', 'd', 'g', 'g'],
               ['f', 'h', 'i', 'i']],
           [['a', 6, '*'],
            ['b', 3, '-'],
            ['c', 3, '='],
            ['d', 5, '+'],
            ['e', 3, '-'],
            ['f', 3, '-'],
            ['g', 2, '/'],
            ['h', 4, '='],
            ['i', 1, '-']]),
    Puzzle(4, [[Guess('a', 2), 'b', 'b', 'c'],
               ['a', 'd', 'e', 'e'],
               ['f', 'd', 'g', 'g'],
               ['f', 'h', 'i', 'i']],
           [['a', 6, '*'],
            ['b', 3, '-'],
            ['c', 3, '='],
            ['d', 5, '+'],
            ['e', 3, '-'],
            ['f', 3, '-'],
            ['g', 2, '/'],
            ['h', 4, '='],
            ['i', 1, '-']]),
    Puzzle(4, [[Guess('a', 3), 'b', 'b', 'c'],
               ['a', 'd', 'e', 'e'],
               ['f', 'd', 'g', 'g'],
               ['f', 'h', 'i', 'i']],
           [['a', 6, '*'],
            ['b', 3, '-'],
            ['c', 3, '='],
            ['d', 5, '+'],
            ['e', 3, '-'],
            ['f', 3, '-'],
            ['g', 2, '/'],
            ['h', 4, '='],
            ['i', 1, '-']]),
    Puzzle(4, [[Guess('a', 4), 'b', 'b', 'c'],
               ['a', 'd', 'e', 'e'],
               ['f', 'd', 'g', 'g'],
               ['f', 'h', 'i', 'i']],
           [['a', 6, '*'],
            ['b', 3, '-'],
            ['c', 3, '='],
            ['d', 5, '+'],
            ['e', 3, '-'],
            ['f', 3, '-'],
            ['g', 2, '/'],
            ['h', 4, '='],
            ['i', 1, '-']])]

puzzle2a = Puzzle(4, [[4, 2, 'a', 'a'],
                      ['b', Guess('c', 3), 'a', 4],
                      ['b', Guess('c', 1), Guess('c', 4), 2],
                      [1, Guess('c', 4), Guess('c', 2), 3]],
                  [['c', 96, '*'],
                   ['b', 5, '+'],
                   ['a', 3, '*']])

puzzle2b = Puzzle(4, [[4, 2, 'a', 'a'],
                      ['b', 3, 'a', 4],
                      ['b', 1, 4, 2],
                      [1, 4, 2, 3]],
                  [['b', 5, '+'],
                   ['a', 3, '*']])

puzzle2c = Puzzle(4, [[4, 2, 'a', 'a'],
                      ['b', Guess('c', 3), 'a', 4],
                      ['b', Guess('c', 3), Guess('c', 4), 2],
                      [1, Guess('c', 4), Guess('c', 2), 3]],
                  [['c', 96, '*'],
                   ['b', 5, '+'],
                   ['a', 3, '*']])


## ******** END TESTING VALUES ***************


## ******** DO NOT CHANGE THESE FUNCTIONS ***************


def fill_in_guess(puz, pos, val):
    '''
    Fills in the pos Position of puz's board with a guess with value val.

    fill_in_guess: Puzzle Posn Nat -> Puzzle
    Requires:
       1 <= val <= len(puz.board)
       0 <= pos.x < puz.size
       0 <= pos.y < puz.size
    '''

    res = Puzzle(puz.size, copy.deepcopy(puz.board),
                 copy.deepcopy(puz.constraints))
    tmp = copy.deepcopy(res.board)
    res.board = place_guess(tmp, pos, val)
    return res


def solve_kenken(orig):
    '''
    Finds the solution to a KenKen puzzle, orig, or returns False
    if there is no solution.

    solve-kenken: Puzzle -> (anyof Puzzle False)
    '''

    to_visit = []
    visited = []
    to_visit.append(orig)
    while to_visit != []:
        if find_blank(to_visit[0]) == False:
            return to_visit[0]
        elif to_visit[0] in visited:
            to_visit.pop(0)
        else:
            nbrs = neighbours(to_visit[0])
            new = list(filter(lambda x: x not in visited, nbrs))
            new_to_visit = new + to_visit[1:]
            new_visited = [to_visit[0]] + visited
            to_visit = new_to_visit
            visited = new_visited

    return False


## ******** END OF PROVIDED FUNCTIONS ***************


# part a)

def read_puzzle(fname):
    '''
    Reads information from fname file and
    returns the info as a Puzzle value.

    Effects: Reads from a file

    read_puzzle: Str -> Puzzle
    Requires: a file named fname exists and represents a puzzle as described
              in the project specification.


    Example:
       Assume inp1972.txt contains:
       4
       a b b c
       a d e e
       f d g g
       f h i i
       a 6 *
       b 3 -
       c 3 =
       d 5 +
       e 3 -
       f 3 -
       g 2 /
       h 4 =
       i 1 -
       then read_puzzle("inp1972.txt") =>
            Puzzle(4, [['a','b','b','c'],
                       ['a','d','e','e'],
                       ['f','d','g','g'],
                       ['f','h','i','i']],
                      [['a', 6,'*'],
                       ['b',3,'-'],
                       ['c',3,'='],
                       ['d',5,'+'],
                       ['e',3,'-'],
                       ['f',3, '-'],
                       ['g',2,'/'],
                       ['h',4,'='],
                       ['i',1,'-']])
    '''
    size = 0
    board = []
    constraints = []
    fin = open(fname, 'r')
    next_str = fin.readline()
    while next_str != "":
        entries = next_str.split()
        if (len(entries) == 1) and (entries[0].isdigit()):
            size = int(entries[0])
        elif entries[2] in ['+', '-', '*', '/', '=']:
            constraints = constraints + \
                          [[entries[0], int(entries[1]), entries[2]]]
        else:
            board = board + [entries]
        next_str = fin.readline()
    fin.close()
    return Puzzle(size, board, constraints)
    pass


test1 = read_puzzle("inp1972.txt")
check.expect("Ta1", test1, puzzle1 )  
check.expect("Ta1 size", test1.size, puzzle1.size) 
check.expect("Ta1 board", test1.board, puzzle1.board) 
check.expect("Ta1 constraints", test1.constraints, puzzle1.constraints)


# part b)

def print_sol(puz, fname):
    '''
    Prints the Puzzle puz in fname file

    Effects: Writes to a file

    print_sol: Puzzle Str -> None
    Requires: Puzzle is solved.

    Example:
       puzzle1soln = Puzzle(4,
          [[2,1,4,3],[3,2,1,4],[4,3,2,1],[1,4,3,2]], [])
       print_sol(puzzle1soln, "out1.txt") => None
       and "out1.txt" contains:
       2  1  4  3
       3  2  1  4
       4  3  2  1
       1  4  3  2
    '''
    fout = open(fname, 'w')
    solution = puz.board
    for i in range(puz.size):
        line = ""
        for j in range(puz.size):
            line = line + "  " + str(solution[i][j])
        fout.write(line[2:] + '\n')
    fout.close()
    pass


'''  
result1.txt should contain:
2  1  4  3
3  2  1  4
4  3  2  1
1  4  3  2

'''

check.set_file_exact("out1.txt", "result1.txt")
check.expect("Tb1", print_sol(puzzle1soln, "out1.txt"), None)


# part c)

def find_blank(puz):
    '''
      If no cells are blank, returns False.

      Otherwise, if the first constraint has only guesses
      on the board, returns 'guess'.

      Otherwise, returns the position of the
      first blank space corresponding to
      the first constraint.

    find_blank: Puzzle -> (anyof Posn False 'guess')

    Examples:
       find_blank(puzzle1) => Posn(0, 0)
       find_blank(puzzle1partial2) => Posn(0, 1)
       find_blank(puzzle1partial3) => 'guess'
       find_blank(puzzle1soln) => False
    '''
    board = puz.board
    if puz.constraints == []:
        return False
    first_constraint = puz.constraints[0][0]
    already_guess = False
    for i in range(puz.size):
        for j in range(puz.size):
            if board[i][j] == first_constraint:
                return Posn(j, i)
            elif isinstance(board[i][j], Guess) and \
                (board[i][j].symbol == first_constraint):
                already_guess = True
    if already_guess:
        return "guess"
    pass


check.expect("Tc1", find_blank(puzzle1), Posn(0, 0))
check.expect("Tc2", find_blank(puzzle1partial2), Posn(0, 1))
check.expect("Tc3", find_blank(puzzle1partial3), 'guess')
check.expect("Tc4", find_blank(puzzle1soln), False)


# part d)

def available_vals(puz, pos):
    '''
    Returns a list of distinct valid entries in increasing
    order for the (x,y) position pos, of puz based on
    the row and column constraints. That is, the entries
    are those that do not conflict with any numbers that
    have been filled in or guessed for the same row or
    column as pos.

    (We completely ignore arithmetic constraints here.)

    available_vals: Puzzle Posn -> (listof Nat)
    Requires:
      0 <= pos.x < puz.size
      0 <= pos.y < puz.size

    Examples:
       available_vals(puzzle1partial, Posn(2,2)) => [2, 4]
       available_vals(puzzle1partial3, Posn(0,3)) => [1, 4]
    '''
    board = puz.board
    row = pos.y
    column = pos.x
    valid_entries = list(range(1, puz.size + 1))
    for i in range(puz.size):
        if (type(board[row][i]) == int) and (board[row][i] in valid_entries):
            valid_entries.remove(board[row][i])
        elif isinstance(board[row][i], Guess) and \
                (board[row][i].number in valid_entries):
            valid_entries.remove(board[row][i].number)
    for j in range(puz.size):
        if (type(board[j][column]) == int) and \
                (board[j][column] in valid_entries):
            valid_entries.remove(board[j][column])
        elif isinstance(board[j][column], Guess) and \
                (board[j][column].number in valid_entries):
            valid_entries.remove(board[j][column].number)
    return valid_entries

    pass


check.expect("Td1", available_vals(puzzle1partial, Posn(2,2)), [2, 4])
check.expect("Td2", available_vals(puzzle1partial3, Posn(0,3)), [1, 4])


# part e)

def place_guess(brd, pos, val):
    '''
    Fills in the (x,y) position, pos, of the
    board, brd, with a Guess with value, val.

    place_guess: Board Posn Nat -> Board
    Requires:
        0 <= pos.x < len(brd)
        0 <= pos.y < len(brd)
        1 <= val <= len(brd)
        brd at pos contains either a Str or a Guess

    Example:
       place_guess(puzzle1partial2.board,Posn(0,1),3)
          => puzzle1partial3.board
    '''
    ## A copy of brd is assigned to res without any
    ## aliasing to avoid mutation of brd.
    ## You should update res and return it
    res = copy.deepcopy(brd)
    row = pos.y
    column = pos.x
    if isinstance(res[row][column], Guess):
        symbol = res[row][column].symbol
    else:
        symbol = res[row][column]
    res[row][column] = Guess(symbol, val)
    return res
    pass


check.expect("Te1", place_guess(puzzle1partial2.board, 
       Posn(0,1),3), puzzle1partial3.board)
check.expect("Te2", place_guess(puzzle1partial4a.board, 
       Posn(2,0),4), puzzle1partial4b.board)

## Note that fill_in_guess calls place_guess
check.expect("Te3", fill_in_guess(puzzle1, Posn(3,2),4), 
       Puzzle(4,[['a','b','b','c'],
            ['a','d','e','e'],
            ['f','d','g',Guess('g',4)],
            ['f','h','i','i']], puzzle1.constraints))


# part f)

def guess_valid(puz):
    '''
    Returns True if the Guesses in puz corresponding to the
    symbol in the first constraint satisfy their constraint
    arithmetically and returns False otherwise.

    (We completely ignore row and column constraints here.)

    guess_valid: Puzzle -> Bool
    Requires:
       All occurrences of the symbol of the first
        constraint in puz are Guesses on the board.

    Examples:
       guess_valid(puzzle1partial3) => True
       guess_valid(puzzle1partial4a) => False
       guess_valid(puzzle1partial4b) => True
    '''
    board = puz.board
    list_of_guessed_numbers = []
    first_constraint = puz.constraints[0]
    constraint_symbol = first_constraint[0]
    constraint_value = first_constraint[1]
    constraint_operator = first_constraint[2]
    for i in range(puz.size):
        for j in range(puz.size):
            if isinstance(board[i][j], Guess) and \
                    (board[i][j].symbol == constraint_symbol):
                list_of_guessed_numbers = list_of_guessed_numbers + \
                                          [board[i][j].number]
    if constraint_operator == '+':
        sum_lst = sum(list_of_guessed_numbers)
        if sum_lst == constraint_value:
            return True
        else:
            return False
    elif constraint_operator == '*':
        prod = 1
        for i in range(len(list_of_guessed_numbers)):
            prod = prod * list_of_guessed_numbers[i]
        if prod == constraint_value:
            return True
        else:
            return False
    elif constraint_operator == '-':
        difference = max(list_of_guessed_numbers) - min(list_of_guessed_numbers)
        if difference == constraint_value:
            return True
        else:
            return False
    elif constraint_operator == '/':
        quotient = max(list_of_guessed_numbers) / min(list_of_guessed_numbers)
        if quotient == constraint_value:
            return True
        else:
            return False
    else:
        if list_of_guessed_numbers[0] == constraint_value:
            return True
        else:
            return False
    pass


check.expect("Tf1", guess_valid(puzzle1partial3), True)
check.expect("Tf2", guess_valid(puzzle1partial4a), False)
check.expect("Tf3", guess_valid(puzzle1partial4b), True)


# part g)

def apply_guess(puz):
    '''
    Returns a new puzzle corresponding to converting
    all guesses in puz into their corresponding
    numbers and removes the first constraint from puz's
    list of constraints.

    apply_guess:  Puzzle -> Puzzle
    Requires:
       guess_valid(puz) => True
       Guesses corresponding to the first constraint symbol
       do not violate the row and column restriction

    Example:
       apply_guess(puzzle1partial3) => puzzle1partial4
    '''
    ## A copy of puz is assigned to res without any
    ## aliasing to avoid mutation of puz.
    ## You should update res and return it
    res = Puzzle(puz.size, copy.deepcopy(puz.board),
                 copy.deepcopy(puz.constraints))
    first_constraint = res.constraints[0][0]
    for i in range(res.size):
        for j in range(res.size):
            if isinstance(res.board[i][j], Guess) and \
            (res.board[i][j].symbol == first_constraint):
                res.board[i][j] = res.board[i][j].number
    res.constraints = res.constraints[1:]
    return res
    pass


check.expect("Tg1", apply_guess(puzzle1partial3), puzzle1partial4)


# part h)

def neighbours(puz):
    '''
    Returns a list of next puzzles after puz
    as described in the assignment specification.

    neighbours: Puzzle -> (listof Puzzle)

    Examples:
       neighbours(puzzle1soln) => []
       neighbours(puzzle2a) => [puzzle2b]
    '''
    ## a copy of puz is assigned to tmp without any
    ## aliasing to avoid mutation of puz.
    tmp = Puzzle(puz.size, copy.deepcopy(puz.board),
                 copy.deepcopy(puz.constraints))
    if not find_blank(tmp):
        return []
    elif isinstance(find_blank(tmp), Posn):
        pos = find_blank(tmp)
        available_lst = available_vals(tmp, pos)
        available_lst.sort()
        puzzles_lst = []
        for i in range(len(available_lst)):
            filled = fill_in_guess(tmp, pos, available_lst[i])
            puzzles_lst = puzzles_lst + [filled]
        return puzzles_lst
    elif (find_blank(tmp) == 'guess') and guess_valid(tmp):
        return [apply_guess(tmp)]
    else:
        return []
    pass


check.expect("Th1", neighbours(puzzle1soln), [])
check.expect("Th2", neighbours(puzzle1), puzzle1_first_guess)
check.expect("Th3", neighbours(puzzle2a),[puzzle2b])
check.expect("Th4", neighbours(puzzle2c), [])


##Final Tests:


check.expect("game1",solve_kenken(puzzle1), puzzle1soln)
check.expect("game2",solve_kenken(puzzle1partial), puzzle1soln)
check.expect("game3",solve_kenken(puzzle1partial2), puzzle1soln)
check.expect("game4",solve_kenken(puzzle1partial3), puzzle1soln)
check.expect("game5",solve_kenken(puzzle1partial4), puzzle1soln)
check.expect("game6 (fail)",solve_kenken(puzzle1partial4a), False)
check.expect("game7",solve_kenken(puzzle1partial4b), puzzle1soln)
check.expect("game8",solve_kenken(puzzle1soln), puzzle1soln)




