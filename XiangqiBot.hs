-- module (NICHT ÄNDERN!)
module XiangqiBot
  ( getMove,
    listMoves,
  )
where

import Data.Char
import Data.List
import Data.Set (fromList, toList)
-- More modules may be imported
import Util

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove a = do
  -- YOUR IMPLEMENTATION HERE
  let currentPlayerMoves = getCurrentPlayerMoves a
  case sHead currentPlayerMoves of
    Nothing -> ""
    Just move -> move

listMoves :: String -> String
listMoves b = do
  -- YOUR IMPLEMENTATION HERE
  let currentPlayerMoves = getCurrentPlayerMoves b
  let formattedMoves = "[" ++ intercalate "," currentPlayerMoves ++ "]"
  formattedMoves

-- YOUR IMPLEMENTATION FOLLOWS
type Position = (Int, Int)

data PieceType
  = Rook
  | Horse
  | Elephant
  | Advisor
  | General
  | Cannon
  | Soldier

data Piece = Piece {pieceType :: PieceType, color :: Color, position :: Position}

data Color = Black | Red

instance Show PieceType where
  show Rook = "r"
  show Horse = "h"
  show Elephant = "e"
  show Advisor = "a"
  show General = "g"
  show Cannon = "c"
  show Soldier = "s"

instance Eq PieceType where
  Rook == Rook = True
  Horse == Horse = True
  Elephant == Elephant = True
  Advisor == Advisor = True
  General == General = True
  Cannon == Cannon = True
  Soldier == Soldier = True
  _ == _ = False

instance Eq Piece where
  p1 == p2 = pieceType p1 == pieceType p2 && color p1 == color p2 && position p1 == position p2

instance Show Piece where
  show (Piece pt Black pos) = show pt ++ ": " ++ show pos
  show (Piece pt Red pos) = map toUpper (show pt) ++ ": " ++ show pos

instance Show Color where
  show Black = "Black"
  show Red = "Red"

instance Eq Color where
  Black == Black = True
  Red == Red = True
  _ == _ = False

getCurrentPlayerMoves :: String -> [String]
getCurrentPlayerMoves b = do
  let clr = if getNextPlayer b == 'r' then Red else Black
  let enemyClr = if clr == Red then Black else Red
  let enemyPieces = getPieces enemyClr b
  let attackingPieces = getGeneralThreateningPieces clr b
  let generalPos = searchGeneral clr b
  let generalAvoid = case generalPos of
        Nothing -> []
        Just genPos -> map (\newPos -> (genPos, newPos)) $ legalMoves (Piece General clr (genPos)) b \\ concat (map (\p -> legalMoves p b) enemyPieces) -- say (1,3) can go to (1,4) but enemy piece can go to (1,4) then remove this
  let toRemoveDuplicates =
        if checkGeneral clr b -- if general is in check
          then
            if length attackingPieces > 1
              then generalAvoid -- general must move to a safe position, when there are more than 1 attacking pieces
              else generalAvoid ++ (map (\(p, newPos) -> (position p, newPos)) $ filter (\(p, newPos) -> not $ checkGeneral clr (updateFen p newPos b)) $ concat $ map (\p -> tp p (legalMoves p b)) (getPieces clr b))
          else map (\(p, newPos) -> (position p, newPos)) $ concat $ map (\p -> tp p (legalMoves p b)) (getPieces clr b)

  map (\(cur, des) -> formatPosition cur ++ "-" ++ formatPosition des) $ sort $ removeDuplicates toRemoveDuplicates

-- creates tuples of piece and its possible positions
tp :: Piece -> [Position] -> [(Piece, Position)]
tp _ [] = []
tp pc (p : ps) = [(pc, p)] ++ tp pc ps

-- move piece to a new position and returns updated fen
updateFen :: Piece -> Position -> String -> String
updateFen p newPos oldFen = do
  let updatedPieces =
        map (\x -> if x == p then (Piece (pieceType p) (color p) newPos) else x) $
          filter
            (\p -> not (position p == newPos)) -- remove piece standing on the new position
            (getAllPieces oldFen)
  let newBoard = sortBy (\p1 p2 -> compare (position p1) (position p2)) updatedPieces
  let boardToFen = loopFen [(x, y) | x <- [0 .. 9], y <- [0 .. 8]] newBoard ""
  intercalate "/" (map (\str -> replaceXwithNum str 0 "") (spl boardToFen [])) ++ " b"

getGeneralThreateningPieces :: Color -> String -> [Piece]
getGeneralThreateningPieces clr fen = do
  let enemyClr = if clr == Red then Black else Red
  let generalPos = searchGeneral clr fen
  case generalPos of
    Nothing -> []
    Just pos -> filter (\p -> pos `elem` attackingMoves p fen) (getPieces enemyClr fen)

--------------- helper functions to create fen ------------------
loopFen [] _ str = str
loopFen (x : xs) newBoard str = if x `elem` map (position) newBoard then (loopFen xs newBoard str1) else (loopFen xs newBoard str2)
  where
    str1 = str ++ (take 1 $ show (head $ filter (\p -> (position p) == x) newBoard))
    str2 = str ++ "X"

spl :: String -> [String] -> [String]
spl [] l' = l'
spl l l' = spl (drop 9 l) l' ++ [(take 9 l)]

replaceXwithNum [] _ str = str
replaceXwithNum (x : xs) empty str =
  if x == 'X' && take 1 xs /= "X"
    then replaceXwithNum xs (0) (str ++ (show $ empty + 1))
    else
      if x == 'X' && take 1 xs == "X"
        then replaceXwithNum xs (empty + 1) str
        else replaceXwithNum xs (empty) (str ++ [x])

---------------------------------------------------------------------

-- finde heraus, ob r oder b am Zug ist
getNextPlayer :: String -> Char
getNextPlayer = last

-- init removes the whitespace and the player's turn // get only board situation
readState :: String -> [String]
readState x = reverse (splitOn '/' (init (init x)))

formatPosition :: Position -> String
formatPosition pos = toLetter (snd pos) ++ show (fst pos)

loopRow :: [String] -> Int -> Int -> [Piece] -> [Piece] -> [Piece]
loopRow [] _ _ _ _ = []
loopRow (str : strs) x y bp rp = (loopColumn str x y bp rp) ++ (loopRow strs (x + 1) y bp rp)

-- reversed Fen -> x -> y -> Black Pieces -> Red Pieces
loopColumn :: String -> Int -> Int -> [Piece] -> [Piece] -> [Piece]
loopColumn [] _ _ bp rp = bp ++ rp
loopColumn (p : ps) x y bp rp = case p of
  'r' -> loopColumn ps x (y + 1) (bp ++ [Piece Rook Black (x, y)]) rp
  'h' -> loopColumn ps x (y + 1) (bp ++ [Piece Horse Black (x, y)]) rp
  'e' -> loopColumn ps x (y + 1) (bp ++ [Piece Elephant Black (x, y)]) rp
  'a' -> loopColumn ps x (y + 1) (bp ++ [Piece Advisor Black (x, y)]) rp
  'g' -> loopColumn ps x (y + 1) (bp ++ [Piece General Black (x, y)]) rp
  'c' -> loopColumn ps x (y + 1) (bp ++ [Piece Cannon Black (x, y)]) rp
  's' -> loopColumn ps x (y + 1) (bp ++ [Piece Soldier Black (x, y)]) rp
  'R' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Rook Red (x, y)])
  'H' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Horse Red (x, y)])
  'E' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Elephant Red (x, y)])
  'A' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Advisor Red (x, y)])
  'G' -> loopColumn ps x (y + 1) bp (rp ++ [Piece General Red (x, y)])
  'C' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Cannon Red (x, y)])
  'S' -> loopColumn ps x (y + 1) bp (rp ++ [Piece Soldier Red (x, y)])
  '1' -> loopColumn ps x (y + 1) bp rp
  '2' -> loopColumn ps x (y + 2) bp rp
  '3' -> loopColumn ps x (y + 3) bp rp
  '4' -> loopColumn ps x (y + 4) bp rp
  '5' -> loopColumn ps x (y + 5) bp rp
  '6' -> loopColumn ps x (y + 6) bp rp
  '7' -> loopColumn ps x (y + 7) bp rp
  '8' -> loopColumn ps x (y + 8) bp rp
  '9' -> loopColumn ps x y bp rp

getBlackPieces :: String -> [Piece]
getBlackPieces fen = filter (\x -> color x == Black) (loopRow (readState fen) 0 0 [] [])

getRedPieces :: String -> [Piece]
getRedPieces fen = filter (\x -> color x == Red) (loopRow (readState fen) 0 0 [] [])

getAllPieces :: String -> [Piece]
getAllPieces fen = getBlackPieces fen ++ getRedPieces fen

getPieces :: Color -> String -> [Piece]
getPieces clr fen = filter (\x -> color x == clr) (getAllPieces fen)

legalMoves :: Piece -> String -> [Position]
legalMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let enemyPieces = if color p == Red then getBlackPieces fen else getRedPieces fen

  let toFilter =
        case pieceType p of
          Rook -> rookMoves p fen
          Cannon -> ((rookMoves p fen) \\ (map (position) enemyPieces)) ++ cannonCapture p fen -- remove rook capturing capabilities
          Elephant -> elephantMoves p fen
          Advisor -> advisorMoves p fen
          Soldier -> soldierMoves p fen
          Horse -> horseMoves p fen
          General -> generalMoves p fen

  -- filter all moves that leaves the general vulnerable, remove all same colored positions
  if pieceType p == General
    then filter (\newPos -> (not $ checkGeneral (color p) (updateFen p newPos fen))) toFilter
    else filter (\newPos -> (not $ checkGeneral (color p) (updateFen p newPos fen)) && (not $ todesBlickCheck (position p) newPos fen)) toFilter

rookMoves :: Piece -> String -> [Position]
rookMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let enemyPieces = if color p == Red then getBlackPieces fen else getRedPieces fen

  let oben =
        -- check if there are any pieces along the way. If there is, get the nearest piece and create empty points along the way
        if (filter (\(x, y) -> x > xOld) $ map (position) $ getAllPieces fen) `intersect` (verticalSquares (position p)) == []
          then [(x, yOld) | x <- [xOld + 1 .. 9]]
          else [(x, yOld) | x <- [xOld + 1 .. fst (getNearestPieceVertical xOld $ (filter (\(x, y) -> x > xOld) $ map (position) $ getAllPieces fen) `intersect` (verticalSquares (position p)))]]
  let unten =
        if (filter (\(x, y) -> x < xOld) $ map (position) $ getAllPieces fen) `intersect` (verticalSquares (position p)) == []
          then [(x, yOld) | x <- [0 .. xOld - 1]]
          else [(x, yOld) | x <- [fst (getNearestPieceVertical xOld $ (filter (\(x, y) -> x < xOld) $ map (position) $ getAllPieces fen) `intersect` (verticalSquares (position p))) .. xOld - 1]]
  let links =
        if (filter (\(x, y) -> y < yOld) $ map (position) $ getAllPieces fen) `intersect` (horizontalSquares (position p)) == []
          then [(xOld, y) | y <- [0 .. yOld - 1]]
          else [(xOld, y) | y <- [snd (getNearestPieceHorizontal yOld $ (filter (\(x, y) -> y < yOld) $ map (position) $ getAllPieces fen) `intersect` (horizontalSquares (position p))) .. yOld - 1]]
  let rechts =
        if (filter (\(x, y) -> y > yOld) $ map (position) $ getAllPieces fen) `intersect` (horizontalSquares (position p)) == []
          then [(xOld, y) | y <- [yOld + 1 .. 8]]
          else [(xOld, y) | y <- [yOld + 1 .. snd (getNearestPieceHorizontal yOld $ (filter (\(x, y) -> y > yOld) $ map (position) $ getAllPieces fen) `intersect` (horizontalSquares (position p)))]]

  sort $ (oben ++ unten ++ links ++ rechts) \\ (map (position) $ getPieces (color p) fen)

cannonCapture :: Piece -> String -> [Position]
cannonCapture p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)

  let enemyPieces = if color p == Red then getBlackPieces fen else getRedPieces fen
  let horizontalPieces = filter (\pos -> countPiecesBetween pos (position p) fen == 1) (horizontalSquares (position p) `intersect` map (position) enemyPieces)
  let verticalPieces = filter (\pos -> countPiecesBetween pos (position p) fen == 1) (verticalSquares (position p) `intersect` map (position) enemyPieces)
  horizontalPieces ++ verticalPieces

-- Helper function to count how many pieces are between two horizontally/vertically aligned points/squares
countPiecesBetween :: Position -> Position -> String -> Int
countPiecesBetween pos1 pos2 fen = do
  let x1 = fst pos1
  let y1 = snd pos1
  let x2 = fst pos2
  let y2 = snd pos2
  let minX = min x1 x2
  let minY = min y1 y2
  let maxX = max x1 x2
  let maxY = max y1 y2
  if (x1 == x2)
    then length $ filter (\(x, y) -> x == x1 && y `elem` [minY .. maxY]) (map (position) (getAllPieces fen) `intersect` (horizontalSquares pos1)) \\ [pos1, pos2]
    else
      if (y1 == y2)
        then length $ filter (\(x, y) -> y == y1 && x `elem` [minX .. maxX]) (map (position) (getAllPieces fen) `intersect` (verticalSquares pos1)) \\ [pos1, pos2]
        else 0

elephantMoves :: Piece -> String -> [Position]
elephantMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let topRight = if (xOld + 1, yOld + 1) `elem` (filter (\pos -> pos == (xOld + 1, yOld + 1)) (map (position) (getAllPieces fen))) then [] else [(xOld + 2, yOld + 2)]
  let topLeft = if (xOld + 1, yOld - 1) `elem` (filter (\pos -> pos == (xOld + 1, yOld - 1)) (map (position) (getAllPieces fen))) then [] else [(xOld + 2, yOld - 2)]
  let botRight = if (xOld - 1, yOld + 1) `elem` (filter (\pos -> pos == (xOld - 1, yOld + 1)) (map (position) (getAllPieces fen))) then [] else [(xOld - 2, yOld + 2)]
  let botLeft = if (xOld - 1, yOld - 1) `elem` (filter (\pos -> pos == (xOld - 1, yOld - 1)) (map (position) (getAllPieces fen))) then [] else [(xOld - 2, yOld - 2)]

  let beforeRiver = if color p == Red then [(x, y) | x <- [0 .. 4], y <- [0 .. 8]] else [(x, y) | x <- [5 .. 9], y <- [0 .. 8]]
  filter (\pos -> pos `elem` beforeRiver) $ filter (\(x, y) -> x `elem` [0 .. 9] && y `elem` [0 .. 8]) (topRight ++ topLeft ++ botRight ++ botLeft) \\ (map (position) $ getPieces (color p) fen)

advisorMoves :: Piece -> String -> [Position]
advisorMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  filter (\pos -> pos `elem` insidePalace (color p)) [(xOld + 1, yOld + 1), (xOld + 1, yOld - 1), (xOld - 1, yOld + 1), (xOld - 1, yOld - 1)] \\ (map (position) $ getPieces (color p) fen)

soldierMoves :: Piece -> String -> [Position]
soldierMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let beforeRiver = if color p == Red then [(x, y) | x <- [0 .. 4], y <- [0 .. 8]] else [(x, y) | x <- [5 .. 9], y <- [0 .. 8]]
  let possibleMoves =
        if (color p == Red)
          then
            if (position p `elem` beforeRiver)
              then [(xOld + 1, yOld)]
              else [(xOld + 1, yOld), (xOld, yOld - 1), (xOld, yOld + 1)]
          else
            if (position p `elem` beforeRiver)
              then [(xOld - 1, yOld)]
              else [(xOld - 1, yOld), (xOld, yOld - 1), (xOld, yOld + 1)]
  filter (\(x, y) -> x `elem` [0 .. 9] && y `elem` [0 .. 8]) possibleMoves \\ (map (position) $ getPieces (color p) fen)

horseMoves :: Piece -> String -> [Position]
horseMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let up = if (xOld + 1, yOld) `elem` (filter (\pos -> pos == (xOld + 1, yOld)) (map (position) (getAllPieces fen))) then [] else [(xOld + 2, yOld - 1), (xOld + 2, yOld + 1)]
  let down = if (xOld - 1, yOld) `elem` (filter (\pos -> pos == (xOld - 1, yOld)) (map (position) (getAllPieces fen))) then [] else [(xOld - 2, yOld - 1), (xOld - 2, yOld + 1)]
  let left = if (xOld, yOld - 1) `elem` (filter (\pos -> pos == (xOld, yOld - 1)) (map (position) (getAllPieces fen))) then [] else [(xOld + 1, yOld - 2), (xOld - 1, yOld - 2)]
  let right = if (xOld, yOld + 1) `elem` (filter (\pos -> pos == (xOld, yOld + 1)) (map (position) (getAllPieces fen))) then [] else [(xOld + 1, yOld + 2), (xOld - 1, yOld + 2)]
  sort $ filter (\(x, y) -> x `elem` [0 .. 9] && y `elem` [0 .. 8]) (up ++ down ++ left ++ right) \\ (map (position) $ getPieces (color p) fen)

generalMoves :: Piece -> String -> [Position]
generalMoves p fen = do
  let pos = position p
  let xOld = fst (position p)
  let yOld = snd (position p)
  let basicMoves =
        if (color p == Red)
          then
            if (pos == (0, 4) || pos == (1, 3) || pos == (2, 4) || pos == (1, 5))
              then filter (onTheBoard) [(xOld + 1, yOld), (xOld - 1, yOld), (xOld, yOld + 1), (xOld, yOld - 1)]
              else filter (onTheBoard) $ filter (\(x, y) -> (x, y) /= (xOld, yOld)) [(x, y) | x <- [xOld - 1 .. xOld + 1], y <- [yOld - 1 .. yOld + 1]]
          else
            if (pos == (9, 4) || pos == (8, 3) || pos == (7, 4) || pos == (8, 5))
              then filter (onTheBoard) [(xOld + 1, yOld), (xOld - 1, yOld), (xOld, yOld + 1), (xOld, yOld - 1)]
              else filter (onTheBoard) $ filter (\(x, y) -> (x, y) /= (xOld, yOld)) [(x, y) | x <- [xOld - 1 .. xOld + 1], y <- [yOld - 1 .. yOld + 1]]
  filter (\pos -> pos `elem` insidePalace (color p)) basicMoves \\ (map (position) $ getPieces (color p) fen)

onTheBoard :: Position -> Bool
onTheBoard = \(x, y) -> x `elem` [0 .. 9] && y `elem` [0 .. 8]

insidePalace :: Color -> [Position]
insidePalace clr = if clr == Red then [(x, y) | x <- [0 .. 2], y <- [3 .. 5]] else [(x, y) | x <- [7 .. 9], y <- [3 .. 5]]

getNearestPieceVertical :: Int -> [Position] -> Position
getNearestPieceVertical _ (p : []) = p
getNearestPieceVertical x (a : b : ps) = if (abs (x - fst a) < abs (x - fst b)) then getNearestPieceVertical x (a : ps) else getNearestPieceVertical x (b : ps)

getNearestPieceHorizontal :: Int -> [Position] -> Position
getNearestPieceHorizontal _ (p : []) = p
getNearestPieceHorizontal y (a : b : ps) = if (abs (y - snd a) < abs (y - snd b)) then getNearestPieceHorizontal y (a : ps) else getNearestPieceHorizontal y (b : ps)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

toLetter :: Int -> String
toLetter x
  | x == 0 = "a"
  | x == 1 = "b"
  | x == 2 = "c"
  | x == 3 = "d"
  | x == 4 = "e"
  | x == 5 = "f"
  | x == 6 = "g"
  | x == 7 = "h"
  | x == 8 = "i"

horizontalSquares :: Position -> [Position]
horizontalSquares pos = [(fst pos, y) | y <- [0 .. 8]]

-- gets Squares that are vertical towards a given position
verticalSquares :: Position -> [Position]
verticalSquares pos = [(x, snd pos) | x <- [0 .. 9]]

searchGeneral :: Color -> String -> Maybe Position
searchGeneral clr fen = do
  sHead $ map (position) $ filter (\p -> color p == clr && pieceType p == General) (getPieces clr fen)

checkGeneral :: Color -> String -> Bool
checkGeneral clr fen = do
  let generalPos = searchGeneral clr fen
  let enemyPieces = if clr == Red then getPieces Black fen else getPieces Red fen

  case generalPos of
    Nothing -> True
    Just pos -> pos `elem` (concat $ map (\p -> attackingMoves p fen) enemyPieces)

sHead :: [a] -> Maybe a
sHead [] = Nothing
sHead (a : as) = Just a

------------------------------------------------------------------------------------------------------------------

attackingMoves :: Piece -> String -> [Position]
attackingMoves p fen = do
  let xOld = fst (position p)
  let yOld = snd (position p)
  let enemyPieces = if color p == Red then getBlackPieces fen else getRedPieces fen

  let toFilter = case pieceType p of
        Rook -> rookMoves p fen
        Cannon -> ((rookMoves p fen) \\ (map (position) enemyPieces)) ++ cannonCapture p fen -- remove rook capturing capabilities
        Elephant -> elephantMoves p fen
        Advisor -> advisorMoves p fen
        Soldier -> soldierMoves p fen
        Horse -> horseMoves p fen
        General -> generalMoves p fen

  filter (\newPos -> (not $ todesBlickCheck (position p) newPos fen)) toFilter

-- if true then moving the non-General piece would be illegal
todesBlickCheck :: Position -> Position -> String -> Bool
todesBlickCheck oldPos newPos fen = do
  let redGeneralPos = searchGeneral Red fen
  let blackGeneralPos = searchGeneral Black fen
  case redGeneralPos of
    Nothing -> False
    Just rPos -> case blackGeneralPos of
      Nothing -> False
      Just bPos ->
        if (snd rPos == snd bPos)
          && (oldPos `elem` verticalSquares rPos)
          && (not $ newPos `elem` verticalSquares rPos)
          && (countPiecesBetween rPos bPos fen == 1)
          then True
          else False
