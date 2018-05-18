-- Created by Phillip Tran 999818578
--            Jackie Sun 912072142


type Board  = ([Char], Int)

getDim::[Char] -> Int
getDim board = round (sqrt(fromIntegral  (length board)))

printBoard::[String] -> IO ()
printBoard b 
  | (tail b) == [] = printBoardHelp (head b) 5 1 []
  | otherwise = do
                    printBoardHelp (head b) 5 1 []
                    printBoard (tail b)

printBoardHelp::[Char] -> Int -> Int -> [Char] -> IO ()
printBoardHelp (h:t) dim count acc
  |  null t = do 
                putStrLn (reverse (h:acc))
                putStrLn "&&&&&&&&&&&&&&&&&&&&&&&&"
  |  dim == count = do
                      putStrLn (reverse (h:acc))
                      printBoardHelp t dim 1 []
  |  otherwise = printBoardHelp t dim (count + 1) (h:acc)



generateBoard::Int -> [Char] -> [Char] -> Char -> Int -> [String]
generateBoard index b orig_b color dim    --check if move has not been generate
  | tail b == [] = (cleanUp (generatePossibleBoards orig_b index color dim []) [])
  | head b == color = (cleanUp (generatePossibleBoards orig_b index color (getDim orig_b) []) []) ++ generateBoard (index + 1) (tail b) orig_b color (getDim orig_b)
  | otherwise = generateBoard (index + 1) (tail b) orig_b color (getDim orig_b)

generatePossibleBoards::[Char] -> Int -> Char -> Int -> [Char] -> [String]
generatePossibleBoards b index color dim acc =
      checkUp b index color dim : ((checkLeft b index color dim) : [(checkRight b index color dim)]) --cons the three possible things together

cleanUp::[[Char]]-> [[Char]] -> [String]
cleanUp (head_of_board:tail_of_board) acc
  | tail_of_board == [] && (length head_of_board == 0) = acc
  | tail_of_board == [] = head_of_board:acc
  | (length head_of_board == 0) = cleanUp tail_of_board acc
  | otherwise = cleanUp tail_of_board (head_of_board:acc)

setAt::Int -> Int -> Char -> [Char] -> [Char]
setAt index count element (h:t)
  |  count == index = element:t
  |  otherwise = h:(setAt index (count + 1) element t)

checkLeft::[Char] -> Int -> Char -> Int -> [Char]
checkLeft b index color dim =
  if (b!!index == color) && (b!!(index - 1) == '-')
  then setAt (index - 1) 0 color (setAt index 0 '-' b )
else []

checkRight::[Char] -> Int -> Char -> Int -> [Char]
checkRight b index color dim =
  if (b!!index == color) && (b!!(index + 1) == '-')
then setAt (index + 1) 0 color (setAt index 0 '-' b )
else []

checkUp::[Char] -> Int -> Char -> Int -> [Char]
checkUp b index color dim =
  if (b!!index == color) && (b!!(index + dim) == '-')
then setAt (index + dim) 0 color (setAt index 0 '-' b )
else []
main::IO()


getValue:: [Char] -> Char ->Int -> Board
getValue b color index dim count --count accumulates the value, currently is num of pawns
  |  index == (dim^2)       = (b, count) -- base case, reach end of board, return the board
  |  b!!index == color      = getValue b color (index + 1) (count + 1)
  |  otherwise              = getValue b color (index + 1) count


main = do
  putStrLn "Hello"
  putStrLn "World"
