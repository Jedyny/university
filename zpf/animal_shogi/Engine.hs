{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Engine where

import Control.Applicative (liftA2)
import Data.Array
import Data.Maybe (maybe)
import Data.List (delete)

import Lists

data PieceKind = Chick | Elephant | Giraffe | Hen | Lion 
  deriving (Eq, Read, Show)

data Player = Black | White deriving (Eq, Read, Show)

opponent :: Player -> Player
opponent Black = White
opponent White = Black

data Piece = Piece { 
  owner :: Player, 
  kind :: PieceKind 
} deriving (Eq, Read, Show)

capture :: Piece -> Piece
capture Piece{..} = Piece {
  owner = opponent owner,
  kind = if kind == Hen then Chick else kind
}

promote :: (Int, Int) -> Piece -> Piece
promote (_, y) piece@(Piece color Chick) = 
  let
    promoteLine = if color == Black then 3 else 0
  in if y == promoteLine then (Piece color Hen) else piece
promote _ piece = piece

type Board = Array (Int, Int) (Maybe Piece)

newBoard :: Board
newBoard = listArray ((0, 0), (2, 3)) 
    [Just $ Piece Black Giraffe, Nothing, Nothing, Just $ Piece White Elephant,
     Just $ Piece Black Lion, Just $ Piece Black Chick, Just $ Piece White Chick, 
     Just $ Piece White Lion, Just $ Piece Black Elephant, Nothing,  Nothing,
       Just $ Piece White Giraffe]

isIxValid :: (Int, Int) -> Bool
isIxValid ix = ix `elem` range ((0, 0), (2, 3))

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p 
  | isIxValid p = filter isIxValid $ cross p ++ diagonal p
  | otherwise = error "Invalid index"
 
isAttackedBy :: Board -> Player -> (Int, Int) -> Bool
isAttackedBy board player ix
  | isIxValid ix = any (canMoveToFrom board ix) 
    $ filter (owns board player) $ neighbors ix  
  | otherwise = error "Invalid index"
  
isAttackedByOpponent :: Board -> Player -> (Int, Int) -> Bool
isAttackedByOpponent board player = isAttackedBy board $ opponent player
  
getLionIx :: Board -> Player -> (Int, Int)
getLionIx board player = let
  xs = filter ((== Just (Piece player Lion)) . snd) $ assocs board
  in case xs of 
    [e] -> fst e
    _ -> error "Unable to find the lion"  

isInCheck :: Board -> Player -> Bool
isInCheck board player = isAttackedByOpponent board player lion
  where
    lion = getLionIx board player

isLionCaptured :: Game -> Bool
isLionCaptured Game{..} = (isInCheck board player && cannotEscape &&
  ((length lionHunters > 1) || (not $ attackedByMe $ head lionHunters))) ||
  (cannotEscape && 
    (count (\e -> fmap owner e == Just player) (elems board) == 1) &&
    (count (\e -> owner e == player) toDrop == 0))  
  where
    attackedByOpponent = isAttackedByOpponent board player
    attackedByMe = isAttackedBy (board // [(lion, Nothing)]) player
    cannotEscape = none (not . liftA2 (||) attackedByOpponent ownedByMe)  
      $ neighbors lion
    lion = getLionIx board player
    lionHunters = filterAnd (canMoveToFrom board lion) ownedByOpponent 
      $ neighbors lion
    ownedByMe = owns board player
    ownedByOpponent = owns board $ opponent player 
    
isLionPromoted :: Board -> Player -> Bool
isLionPromoted board player = case player of
  Black -> y == 3
  White -> y == 0 
  where
     (_, y) = getLionIx board player

cross :: (Int, Int) -> [(Int, Int)]
cross (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

diagonal :: (Int, Int) -> [(Int, Int)]
diagonal (x, y) = [(x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]

empty :: Board -> (Int, Int) -> Bool
empty board = (== Nothing) . (board !) 

owns :: Board -> Player -> (Int, Int) -> Bool
owns board player ix = (== Just player) $ fmap owner (board ! ix) 

canMoveToFrom :: Board -> (Int, Int) -> (Int, Int) -> Bool
canMoveToFrom board to from
  | isIxValid from && isIxValid to = case board ! from of
    Just piece -> canMove piece from to
    Nothing -> False
  | otherwise = error "Invalid index"

canMove :: Piece -> (Int, Int) -> (Int, Int) -> Bool
canMove (Piece Black Chick) (x, y) to = to == (x, y + 1)
canMove (Piece Black Hen) (x, y) to = 
  to `elem` cross (x, y) ++ [(x + 1, y + 1), (x - 1, y + 1)]
canMove (Piece White Chick) (x, y) to = to == (x, y - 1)
canMove (Piece White Hen) (x, y) to = 
  to `elem` cross (x, y) ++ [(x - 1, y - 1), (x + 1, y - 1)]
canMove (Piece _ Elephant) from to = to `elem` diagonal from
canMove (Piece _ Giraffe) from to = to `elem` cross from
canMove (Piece _ Lion) from to = to `elem` diagonal from ++ cross from

data Game = Game {
  board :: Board,
  player :: Player,
  toDrop :: [Piece]
} deriving (Read, Show)


newGame = Game {
  board = newBoard,
  player = White,
  toDrop = []
}

getWinner :: Game -> Maybe Player
getWinner game@Game{..} = if (promoted || captured) then Just (opponent player) 
  else Nothing
  where
    promoted = isLionPromoted board $ opponent player
    captured = isLionCaptured game

data Action = 
  Drop { dropped :: Piece, droppedTo :: (Int, Int) } |
  Move { movedFrom :: (Int, Int), movedTo :: (Int, Int) }
  deriving (Read, Show)

isActionValid :: Game -> Action -> Bool
isActionValid Game{..} Drop{..} = player == owner dropped && 
  isIxValid droppedTo && dropped `elem` toDrop && board `empty` droppedTo 
isActionValid Game{..} Move{..} = isIxValid movedFrom && isIxValid movedTo && 
  fmap (owns board player) [movedFrom, movedTo] == [True, False] &&
  canMoveToFrom board movedTo movedFrom &&
  ((fmap kind (board ! movedFrom) /= Just Lion) || 
    (not $ isAttackedByOpponent board player movedTo)) 
  
performAction :: Game -> Action -> (Maybe Game, Maybe Player)
performAction game@Game{board = currBoard, ..} action
  | not $ isActionValid game action = (Nothing, Nothing)
  | otherwise = case action of
    Drop{..} -> let
      nextState = Game { 
        player = opponent player,
        board = currBoard // [(droppedTo, Just dropped)],
        toDrop = delete dropped toDrop
      } in if (isStillInCheck nextState) then (Nothing, Nothing) 
          else (Just nextState, getWinner nextState) 
    Move{..} -> let
      movedPiece = fmap (promote movedTo) $ currBoard ! movedFrom
      maybeCaptured = fmap capture (currBoard ! movedTo)
      nextState = Game {
        player = opponent player,
        board = currBoard // [(movedFrom, Nothing), (movedTo, movedPiece)],
        toDrop = maybe toDrop (:toDrop) maybeCaptured
      } in if (isStillInCheck nextState) then (Nothing, Nothing) 
          else (Just nextState, getWinner nextState)
  where
    isStillInCheck next = isInCheck currBoard player && 
      isInCheck (board next) player 
