import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random (mkStdGen, randomR)

type Board = [[Maybe Block]]

type Block = Char

type Position = (Int, Int)

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 20

data Piece = I | J | O | S | T | Z deriving (Show, Enum, Bounded)

pieceRotations :: Piece -> [[Position]]
pieceRotations I =
  [ [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(0, 0), (0, 1), (0, 2), (0, 3)]
  ]
pieceRotations J =
  [ [(0, 0), (0, 1), (1, 1), (2, 1)],
    [(0, 0), (1, 0), (0, 1), (0, 2)],
    [(0, 0), (1, 0), (2, 0), (2, 1)],
    [(1, 0), (1, 1), (0, 2), (1, 2)]
  ]
pieceRotations O =
  [ [(0, 0), (0, 1), (1, 0), (1, 1)],
    [(0, 0), (0, 1), (1, 0), (1, 1)],
    [(0, 0), (0, 1), (1, 0), (1, 1)],
    [(0, 0), (0, 1), (1, 0), (1, 1)]
  ]
pieceRotations S =
  [ [(0, 1), (1, 0), (1, 1), (2, 0)],
    [(0, 0), (0, 1), (1, 1), (1, 2)],
    [(0, 1), (1, 0), (1, 1), (2, 0)],
    [(0, 0), (0, 1), (1, 1), (1, 2)]
  ]
pieceRotations T =
  [ [(0, 0), (0, 1), (0, 2), (1, 1)],
    [(0, 1), (1, 0), (1, 1), (2, 1)],
    [(0, 0), (0, 1), (0, 2), (1, 1)],
    [(0, 0), (1, 0), (1, 1), (2, 0)]
    -- dd  1 0 0
    --  1 1 0
    --  1 0 0
  ]
pieceRotations Z =
  [ [(0, 0), (1, 0), (1, 1), (2, 1)],
    [(0, 1), (0, 2), (1, 0), (1, 1)],
    [(0, 0), (1, 0), (1, 1), (2, 1)],
    [(0, 1), (0, 2), (1, 0), (1, 1)]
  ]

placePieceOnBoard :: Piece -> Int -> Position -> Board -> (Board, Bool)
placePieceOnBoard piece rotation (row, col) board =
  -- Update the board with the piece's blocks
  foldl' placeBlock (board, False) ((pieceRotations piece !! rotation))
  where
    placeBlock :: (Board, Bool) -> Position -> (Board, Bool)
    placeBlock (b, overlap) (r, c) =
      let newPos = (row + r, col + c)
          newOverlap = overlap || isJust (getBlockAt b newPos)
       in (updateBoardAt b newPos (Just (pieceChar piece)), newOverlap)

    updateBoardAt :: Board -> Position -> Maybe Block -> Board
    updateBoardAt b (r, c) val
      | r >= 0 && r < boardHeight && c >= 0 && c < boardWidth =
          take r b ++ [take c (b !! r) ++ [val] ++ drop (c + 1) (b !! r)] ++ drop (r + 1) b
      | otherwise = b

    getBlockAt :: Board -> Position -> Maybe Block
    getBlockAt b (r, c)
      | r >= 0 && r < boardHeight && c >= 0 && c < boardWidth = b !! r !! c
      | otherwise = Nothing

    pieceChar :: Piece -> Char
    pieceChar I = 'I'
    pieceChar J = 'J'
    pieceChar O = 'O'
    pieceChar S = 'S'
    pieceChar T = 'T'
    pieceChar Z = 'Z'

renderBoard :: Board -> String
renderBoard board = unlines (map renderRow board)
  where
    renderRow :: [Maybe Block] -> String
    renderRow = concatMap (maybe "⬛" (\_ -> "⬜"))

createNewPiece :: IO Piece
createNewPiece = do
  seed <- round <$> getPOSIXTime
  let g = mkStdGen seed -- Replace 42 with your seed
      (pieceIndex, _) = randomR (0, fromEnum (maxBound :: Piece)) g

  return $ toEnum pieceIndex :: IO Piece

main :: IO ()
main = do
  let initialBoard = replicate boardHeight (replicate boardWidth Nothing) -- Create an empty board
  boardVar <- newTVarIO initialBoard -- Initialize the TVar
  newPiece <- createNewPiece
  currentPiece <- newTVarIO newPiece
  gameLoop boardVar currentPiece (0, boardWidth `div` 2) -- Start in the middle of the top row

calculatePieceWidth :: Piece -> Int -> Int
calculatePieceWidth piece rotation =
  let xs = map snd (pieceRotations piece !! rotation)
   in maximum xs - minimum xs + 1

calculatePieceHeight :: Piece -> Int -> Int
calculatePieceHeight piece rotation =
  let rows = map fst (pieceRotations piece !! rotation)
   in maximum rows - minimum rows + 1

mergeBoards :: Board -> Board -> (Board, Bool)
mergeBoards b1 b2 = (newBoard, any id (concat overlap))
  where
    (newBoard, overlap) = unzip $ zipWith mergeRows b1 b2
    mergeRows r1 r2 = unzip $ zipWith mergeBlocks r1 r2
    mergeBlocks (Just b1) (Just b2) = ((Just b1), True)
    mergeBlocks (Just b1) _ = ((Just b1), False)
    mergeBlocks _ (Just b2) = ((Just b2), False)
    mergeBlocks _ _ = (Nothing, False)

clearanceOnDrop :: TVar Board -> TVar Int -> STM ()
clearanceOnDrop boardVar scoreVar = do
  -- If there are any rows which are full, remove them and drop the rows above
  board <- readTVar boardVar
  -- Remove the full rows
  let newBoard = filter (not . all isJust) board
  -- Add new rows to the top

  -- calculate the number of rows to add
  let rowsToAdd = boardHeight - length newBoard

  -- add to score
  modifyTVar' scoreVar (\x -> x + rowsToAdd * 100)

  let newBoardWithRows = replicate (length board - length newBoard) (replicate boardWidth Nothing) ++ newBoard
  -- Update the board

  modifyTVar' boardVar (\_ -> newBoardWithRows)

updateBoard :: Char -> TVar Piece -> TVar Position -> TVar Int -> TVar Board -> TVar Int -> IO ()
updateBoard input pieceVar posVar rotationVar boardVar scoreVar = do
  rotation <- readTVarIO rotationVar
  newPiece <- createNewPiece
  atomically $ do
    pos <- readTVar posVar
    board <- readTVar boardVar
    piece <- readTVar pieceVar
    let pieceWidth = calculatePieceWidth piece rotation
    let pieceHeight = calculatePieceHeight piece rotation

    let newPos = handleInput input pos pieceWidth

    let (_, overlap) = placePieceOnBoard piece rotation newPos board

    let condition = overlap || fst newPos > boardHeight - pieceHeight
    -- If there are overlaps or the piece is at the bottom, start a new piece
    if condition
      then do
        let (newBoard, _) = placePieceOnBoard piece rotation pos board
        let (mergedBoard, _) = mergeBoards newBoard board
        -- write the board using the previous position
        modifyTVar' boardVar (\_ -> mergedBoard)
        writeTVar pieceVar newPiece
        writeTVar posVar (0, boardWidth `div` 2)
        clearanceOnDrop boardVar scoreVar
      else writeTVar posVar newPos

renderGameBoard :: TVar Piece -> TVar Position -> TVar Int -> TVar Board -> TVar Int -> IO ()
renderGameBoard pieceVar posVar rotationVar boardVar scoreVar = do
  pos <- readTVarIO posVar
  piece <- readTVarIO pieceVar
  board <- readTVarIO boardVar
  rotation <- readTVarIO rotationVar
  score <- readTVarIO scoreVar

  -- place the piece on the board
  let (newBoard, _) = placePieceOnBoard piece rotation pos board

  clearScreen
  setCursorPosition 0 0
  --   putStrLn $ "Score: " ++ show score

  -- print score in the right corner

  -- print game instructions here
  putStr "Game Instructions: "
  putStr "a: move left, "
  putStr "d: move right, "
  putStr "s: move down, "
  putStr "q: rotate left, "
  putStr "e: rotate right "
  putStr "CTRL + D: quit game"

  setCursorPosition 2 (boardWidth + 2)

  putStrLn $ "Score: " ++ show score

  putStr $ renderBoard newBoard

gameLoop :: TVar Board -> TVar Piece -> Position -> IO ()
gameLoop boardVar pieceVar pos = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  posVar <- newTVarIO pos
  rotationVar <- newTVarIO 0

  scoreVar <- newTVarIO 0

  -- Visual thread
  _ <- forkIO $ forever $ do
    threadDelay 16666
    renderGameBoard pieceVar posVar rotationVar boardVar scoreVar

  -- Start a new thread to move the piece down every second
  _ <- forkIO $ forever $ do
    pos_temp <- readTVarIO posVar
    board <- readTVarIO boardVar
    piece <- readTVarIO pieceVar

    updateBoard 's' pieceVar posVar rotationVar boardVar scoreVar
    threadDelay 500000

  -- In the main thread, listen for the user's input to move the piece left or right
  forever $ do
    pos_temp <- readTVarIO posVar
    input <- getChar
    piece <- readTVarIO pieceVar

    -- switch cases for input
    case input of
      'a' -> updateBoard input pieceVar posVar rotationVar boardVar scoreVar
      'd' -> updateBoard input pieceVar posVar rotationVar boardVar scoreVar
      's' -> updateBoard input pieceVar posVar rotationVar boardVar scoreVar
      'q' -> do
        atomically $ do
          rotation <- readTVar rotationVar
          let newRotation = (rotation - 1) `mod` 4
          -- calculate the new width and height of the piece and check if it is out of bounds or overlaps
          let pieceWidth = calculatePieceWidth piece newRotation
          let pieceHeight = calculatePieceHeight piece newRotation

          -- check if the piece is out of bounds
          if fst pos_temp > boardHeight - pieceHeight || snd pos_temp > boardWidth - pieceWidth
            then return ()
            else do
              board <- readTVar boardVar
              let (_, overlap) = placePieceOnBoard piece newRotation pos_temp board
              if overlap
                then return ()
                else writeTVar rotationVar newRotation
      'e' -> do
        atomically $ do
          rotation <- readTVar rotationVar
          let newRotation = (rotation + 1) `mod` 4
          -- calculate the new width and height of the piece and check if it is out of bounds or overlaps
          let pieceWidth = calculatePieceWidth piece newRotation
          let pieceHeight = calculatePieceHeight piece newRotation

          -- check if the piece is out of bounds
          if fst pos_temp > boardHeight - pieceHeight || snd pos_temp > boardWidth - pieceWidth
            then return ()
            else do
              board <- readTVar boardVar
              let (_, overlap) = placePieceOnBoard piece newRotation pos_temp board
              if overlap
                then return ()
                else writeTVar rotationVar newRotation
      _ -> return ()

-- pivot point the the top left corner of the piece
--  Calculate the width of the piece
handleInput :: Char -> Position -> Int -> Position
handleInput input pos pieceWidth = case input of
  'a' -> if snd pos > 0 then (fst pos, snd pos - 1) else pos -- Move left
  'd' -> if snd pos < boardWidth - pieceWidth then (fst pos, snd pos + 1) else pos -- Move right
  's' -> (fst pos + 1, snd pos) -- Move down
  _ -> pos
