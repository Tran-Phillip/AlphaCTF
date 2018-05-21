-- Created by Phillip Tran 999818578
--            Jackie Sun 912072142
import Data.Char

type Board  = (String, Int)

test::Board  -> Char -> Int -> [String] -> IO()
test b color lookAhead prev
    | lookAhead == 0 = do
                          putStrLn "Done!"

    | otherwise = do
                    (capture2 [(fst b)] color lookAhead ["w-Ww--www-------bbb--bB-b"])



capture2::[String]-> Char -> Int -> [String] -> IO()
capture2 b color lookAhead prev =
   do
     print (capture ([fst (capture b 'w' 0 prev)]) 'b' lookAhead prev)
     test (capture ([fst (capture b 'w' 0 prev)]) 'b' lookAhead prev) color (lookAhead - 1) (fst (capture ([fst (capture b 'w' 0 prev)]) 'b' lookAhead prev):prev)

capture::[String]-> Char -> Int -> [String] -> Board
capture b color lookAhead prev =
  capture_helper (move 0 (head b) color (getDim (head b)) prev) color lookAhead (head b, 10000)


capture_helper::[Board] -> Char -> Int -> Board -> Board
capture_helper boards color lookAhead localMin
  |  (tail boards) == [] && (snd localMin) == 10000 = (head boards) -- 'go right' in our case
  |  (tail boards) == [] = localMin --base case
  |  snd (head boards) < (snd localMin) = capture_helper (tail boards) color lookAhead (head boards) --update the loca min
  |  otherwise = capture_helper (tail boards) color lookAhead localMin


getDim::String -> Int
getDim board = round (sqrt(fromIntegral  (length board)))



printBoard::[Board] -> IO ()
printBoard b
  | (tail b) == [] = do
                    putStrLn "BOARD:"
                    print (snd (head b))
                    printBoardHelp (fst (head b)) 5 1 []
  | otherwise = do
                    putStrLn "BOARD: \n"
                    print (snd (head b))
                    printBoardHelp (fst (head b)) 5 1 []
                    printBoard (tail b)

printBoardHelp::String -> Int -> Int -> String -> IO ()
printBoardHelp (h:t) dim count acc
  |  null t = do
                putStrLn (reverse (h:acc))
                putStrLn "&&&&&&&&&&&&&&&&&&&&&&&&"
  |  dim == count = do
                      putStrLn (reverse (h:acc))
                      printBoardHelp t dim 1 []
  |  otherwise = printBoardHelp t dim (count + 1) (h:acc)

reverseAll::[Board] -> [Board]
reverseAll b
  | tail b == [] = [((reverse (fst (head b))) , (snd (head b)))]
  | otherwise = (reverse (fst (head b)), (snd (head b))):reverseAll (tail b)


move::Int -> String  -> Char -> Int -> [String] -> [Board]
move index b color dim prev_boards
 | color == 'w' =  (generateBoard index b b color dim prev_boards)
 | color == 'b' = (reverseAll (generateBoard index (reverse b) (reverse b) color dim prev_boards))

generateBoard::Int -> String -> String -> Char -> Int -> [String] ->[Board]
generateBoard index b orig_b color dim prev_boards    --On it boss.
  | tail b == [] = (cleanUp (generatePossibleBoards orig_b index color dim [] prev_boards) [])
  | head b == color || head b == toUpper color  = (cleanUp (generatePossibleBoards orig_b index color (getDim orig_b) [] prev_boards) []) ++ generateBoard (index + 1) (tail b) orig_b color (getDim orig_b) prev_boards
  | otherwise = generateBoard (index + 1) (tail b) orig_b color (getDim orig_b) prev_boards

generatePossibleBoards::String -> Int -> Char -> Int -> [String] -> [String] -> [Board]
generatePossibleBoards b index color dim acc prev_boards =
    (getValue (eatRight b index color dim prev_boards) color 0 dim 0):(getValue(eatLeft b index color dim prev_boards) color 0 dim 0) :getValue(eatMeDaddy b index color dim prev_boards) color 0 dim 0 :(getValue (checkUp b index color dim prev_boards) color 0 dim 0 : (getValue (checkLeft b index color dim prev_boards) color 0 dim 0): ([getValue (checkRight b index color dim prev_boards) color 0 dim 0])) --cons the three possible things together

cleanUp::[Board]-> [Board] -> [Board]
cleanUp (head_of_board:tail_of_board) acc  --Goes through our generated boards and removes all the empty ones
  | tail_of_board == [] && (length (fst head_of_board) == 0) = acc
  | tail_of_board == [] = head_of_board:acc
  | (length (fst head_of_board) == 0) = cleanUp tail_of_board acc
  | otherwise = cleanUp tail_of_board (head_of_board:acc)

setAt::Int -> Int -> Char -> String -> String
setAt index count element (h:t)
  |  count == index = element:t
  |  otherwise = h:(setAt index (count + 1) element t)

checkExistance::String -> [String] -> String
checkExistance board prev_boards --checks to see if this board already exist
  |  prev_boards == [] = board -- nothing in our list
  |  (head prev_boards) == [] = board --end of the list and havent seen the board
  |  (head prev_boards == board) = [] -- seen the board already
  |   otherwise = checkExistance board (tail prev_boards) -- we keep going homie.

checkLeft::String -> Int -> Char -> Int -> [String]->String
checkLeft b index color dim prev_boards = --can I move left?
  if (index `mod` dim /= 0 && b!!index == color ) && (b!!(index - 1) == '-')
  then (checkExistance (setAt (index - 1) 0 color (setAt index 0 '-' b )) prev_boards)
else []

checkRight::String -> Int -> Char -> Int -> [String] -> String
checkRight b index color dim prev_boards= --can I move right?
  if (index `mod` dim /= dim - 1 && b!!index == color ) && (b!!(index + 1) == '-')
then (checkExistance  ( setAt (index + 1) 0 color (setAt index 0 '-' b ) ) prev_boards )
else []

checkUp::String -> Int -> Char -> Int -> [String] ->String
checkUp b index color dim prev_boards = --can I move up?
  if ((index < (dim^2 - dim) && b!!index == color ) && (b!!(index + dim) == '-'))
then (checkExistance (setAt (index + dim) 0 color (setAt index 0 '-' b )) prev_boards)
else []

eatMeDaddy::String -> Int -> Char -> Int -> [String] ->String -- we should probably change the name of this, but in case we forget I hope the TA gets a good laugh.
eatMeDaddy b index color dim prev_boards =
  if (index < (dim^2 - dim) && b!!index == color && b!!(index + dim) /= '-' && b!!(index + dim) /= color  && b!!(index + dim) /= (toUpper color) && b!!(index + (2 * dim)) == '-')
    then (checkExistance (setAt (index + dim) 0 '-' (setAt (index + (2 * dim)) 0 color (setAt index 0 '-' b ))) prev_boards)
  else []

eatLeft::String -> Int -> Char -> Int -> [String] ->String
eatLeft b index color dim prev_boards =
  if (index `mod` dim /= 0 && b!!index == color  && b!!(index - 1) /= '-' && (b!!(index - 1) /= color && b!!(index - 1) /= (toUpper color))  && b!!(index - 2) == '-')
    then (checkExistance (setAt (index - 1) 0 '-' (setAt (index - 2) 0 color (setAt index 0 '-' b ))) prev_boards)
  else []

eatRight::String -> Int -> Char -> Int -> [String] ->String
eatRight b index color dim prev_boards
  | (index `mod` dim /= dim - 1 && b!!index == color && b!!(index + 1) /= '-' && (b!!(index + 1) /= color && b!!(index + 1) /= (toUpper color)) && b!!(index + 2) == '-') = (checkExistance (setAt (index + 1) 0 '-' (setAt (index + 2) 0 color (setAt index 0 '-' b ))) prev_boards)
  | otherwise =  []

moveFlag::String -> Int -> Char -> Int -> [String] -> [String] -> [Board]
moveFlag b index color dim acc prev_boards =
    (getValue (moveBack b index (toUpper color) dim prev_boards) color 0 dim 0):(getValue (checkUp b index (toUpper color) dim prev_boards) color 0 dim 0 : (getValue (checkLeft b index (toUpper color) dim prev_boards) color 0 dim 0): ([getValue (checkRight b index (toUpper color) dim prev_boards) color 0 dim 0])) --cons the three possible things together
moveBack::String -> Int -> Char -> Int -> [String] ->String  --special move only for flag
moveBack b index color dim prev_boards
  | (index >= dim) && b!!index == (toUpper color) && b!!(index - dim) == '-' = (checkExistance (setAt (index - dim ) 0 (toUpper color) (setAt index 0 '-' b )) prev_boards)
  | otherwise = []
main::IO()


getValue:: String -> Char ->Int -> Int -> Int -> Board
getValue b color index dim count --count accumulates the value, currently is num of pawns
  |  b == [] = ("", 0) --other base case. Maybe
  |  index == (dim^2)       = (b, count) -- base case, reach end of board, return the board
  |  b!!index == 'b' || b!!index == toUpper 'b'      = getValue b color (index + 1) dim (count + 1)
  |  otherwise              = getValue b color (index + 1) dim count


main = do
  putStrLn "Hello"
  putStrLn "World"
