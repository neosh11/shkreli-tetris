module Main where
import System.IO (hSetEcho, hSetBuffering, BufferMode(NoBuffering), stdin)
import System.Random (newStdGen, randomR)

type Board = [[Maybe Block]]
type Block = Char
type Position = (Int, Int)

-- Dimensions of the board
boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 20

-- Initialize an empty board
initBoard :: Board
initBoard = replicate boardHeight (replicate boardWidth Nothing)

-- Define the Tetris pieces
data Piece = I | J | L | O | S | T | Z deriving (Show, Enum, Bounded)

-- Define the shapes of the pieces
pieceShape :: Piece -> [Position]
pieceShape I = [(0,1), (1,1), (2,1), (3,1)]
pieceShape J = [(0,0), (0,1), (1,1), (2,1)]
pieceShape L = [(0,1), (1,1), (2,1), (2,0)]
pieceShape O = [(0,0), (0,1), (1,0), (1,1)]
pieceShape S = [(1,0), (1,1), (0,1), (0,2)]
pieceShape T = [(0,1), (1,0), (1,1), (1,2)]
pieceShape Z = [(0,0), (0,1), (1,1), (1,2)]


placePieceOnBoard :: Piece -> Position -> Board -> Board
placePieceOnBoard piece (row, col) board = 
  -- Update the board with the piece's blocks
  foldl (placeBlock (pieceChar piece)) board (pieceShape piece)
  where
    placeBlock :: Char -> Board -> Position -> Board
    placeBlock ch b (r, c) = updateBoardAt b (row + r, col + c) (Just ch)

    updateBoardAt :: Board -> Position -> Maybe Block -> Board
    updateBoardAt b (r, c) val
      | r >= 0 && r < boardHeight && c >= 0 && c < boardWidth = 
          take r b ++ [take c (b !! r) ++ [val] ++ drop (c + 1) (b !! r)] ++ drop (r + 1) b
      | otherwise = b

    pieceChar :: Piece -> Char
    pieceChar I = 'I'
    pieceChar J = 'J'
    pieceChar L = 'L'
    pieceChar O = 'O'
    pieceChar S = 'S'
    pieceChar T = 'T'
    pieceChar Z = 'Z'


main :: IO ()
main = do
  g <- newStdGen
  
  let (pieceIndex, _) = randomR (0, fromEnum (maxBound :: Piece)) g
  let currentPiece = toEnum pieceIndex :: Piece
  gameLoop initBoard currentPiece (0, boardWidth `div` 2) -- Start in the middle of the top row

  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  gameLoop initBoard

gameLoop :: Board -> Piece -> Position -> IO ()
gameLoop board piece pos = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  putStrLn $ renderBoard $ placePieceOnBoard piece pos board
  input <- getLine
  let newPos = case input of
        "a" -> (fst pos, snd pos - 1) -- Move left
        "d" -> (fst pos, snd pos + 1) -- Move right
        _   -> pos

  gameLoop initBoard piece newPos

--   let newBoard = handleInput input board -- Define how input affects the board
--   threadDelay 1000000
--   gameLoop newBoard

-- handleInput :: String -> Board -> Board
-- handleInput input board = ... -- Logic to modify the board based on input


