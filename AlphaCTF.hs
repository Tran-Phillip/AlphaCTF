-- Created by Phillip Tran 999818578
--            Jackie Sun 912072142


type Board  = ([Char], Int)

getDim::[Char] -> Int
getDim board = round (sqrt(fromIntegral  (length board)))

printBoard::Board -> IO ()
printBoard b = printBoardHelp (fst b) (getDim (fst b)) 1 []

printBoardHelp::[Char] -> Int -> Int -> [Char] -> IO ()
printBoardHelp (h:t) dim count acc
  |  null t = putStrLn (reverse (h:acc))
  |  dim == count = do
                      putStrLn (reverse (h:acc))
                      printBoardHelp t dim 1 []
  |  otherwise = printBoardHelp t dim (count + 1) (h:acc)



capture::[[Char]] -> Char -> Int -> [Char]
capture history color num = generateBoard 1 (head history) color (getDim (head history))

generateBoard::Int -> [Char] -> Char -> Int -> [Char]
generateBoard index b color dim  --check if move has not been generate

  |  index == (length b) = b
  | (b!!index == color) && (b!!(index - 1) == '-') && ((index `mod` dim) /= 0)  = setAt (index - 1) 0 color (setAt index 0 '-' b )
                                                                                                             --moving to the left Fix me
  | (b!!index == color) && (b!!(index + 1) == '-') && ((index `mod` dim) /= 0)  = setAt (index + 1) 0 color (setAt index 0 '-' b ) --moving to the right Fix me

  | (b!!index == color) && (b!!(index + dim) == '-') && ((index `mod` dim) /= 0)  = setAt (index + dim) 0 color (setAt index 0 '-' b ) --moving to the up Fix me

  | otherwise = generateBoard (index + 1) b color dim --modify this later

setAt::Int -> Int -> Char -> [Char] -> [Char]
setAt index count element (h:t)
  |  count == index = element:t
  |  otherwise = h:(setAt index (count + 1) element t)

main::IO()

main = do
  putStrLn "Hello"
  putStrLn "World"
