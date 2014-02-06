{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

import Control.Monad (void, zipWithM_)
import Data.Array
import Data.Maybe (isJust, maybe)

import Engine
import Identifiers
import Util

customPort :: Int
customPort = 10000

customHtml :: String
customHtml = "shogi.html"

staticDir :: String
staticDir = ""

main :: IO ()
main = do
    startGUI Config
        { tpPort       = customPort
        , tpCustomHTML = Just "shogi.html"
        , tpStatic     = staticDir
        } shogi
        
shogi :: Window -> IO ()
shogi window = do
  
  newGameButton <- (window?) newGameButtonId
  fields <- mapM (window?) boardFieldsIds
  toDrops <- mapM (window?) dropsIds
  
  let networkDescription :: forall t. Frameworks t => Moment t ()
      networkDescription = do
              
      newGames <- event UI.click newGameButton        
      dragFieldEvents <- mapM (event UI.dragStart) fields
      dragToDropEvents <- mapM (event UI.dragStart) toDrops
      dropEvents <- mapM (event UI.drop) fields
      
      let
        dropActionStarts :: Event t ((Int, Int) -> Action)
        dropActionStarts = foldl1 union $ 
          zipWith (\e p -> Drop p <$ e) dragToDropEvents
            (Piece <$> [Black, White] <*> [Chick, Elephant, Giraffe]) 
 
        moveActionStarts :: Event t ((Int, Int) -> Action)
        moveActionStarts = foldl1 union $
          zipWith (\e i -> Move i <$ e) dragFieldEvents
            [(x, y) | y <- [0..3], x <- [0..2]] 
      
        actionStarts :: Event t ((Int, Int) -> Action)
        actionStarts = dropActionStarts `union` moveActionStarts
      
        actionStart :: Behavior t ((Int, Int) -> Action)
        actionStart = stepper (undefined) actionStarts
        
        dropIxs :: Event t (Int, Int)
        dropIxs = foldl1 union $
          zipWith (\e i -> i <$ e) dropEvents
            [(x, y) | y <- [0..3], x <- [0..2]] 
            
        results :: Event t (Maybe Game, Maybe Player)
        results = performAction <$> game <@> (actionStart <@> dropIxs) 
          
        actions :: Event t Game
        actions = Reactive.Banana.filterJust $ fst <$> results    
        
        winners :: Event t Player
        winners = Reactive.Banana.filterJust $ snd <$> results        
            
        game :: Behavior t Game
        game = stepper (newGame) (actions `union` (newGame <$ newGames))
      
      stateChanges <- changes game
      
      reactimate $ hideDialog window <$ newGames      
      reactimate $ draw window <$> stateChanges
      reactimate $ (endGame window) <$> winners     
      --reactimate $ alert window . show <$> results
  
  network <- compile networkDescription
  actuate network    
  
alert w s = runFunction w (ffi "alert(%1)" s)

hideDialog :: Window -> IO ()
hideDialog window = sequence_ [
  window ? newGameDialogId #. hiddenClass,
  window ? scarWinsId #. hiddenClass,
  window ? simbaWinsId #. hiddenClass,
  window ? newGameInfoId #. hiddenClass]

endGame :: Window -> Player -> IO () 
endGame window player = sequence_ [
  window ? (if player == Black then scarWinsId else simbaWinsId) #. "",
  window ? newGameDialogId #. ""]
  
draw :: Window -> Game -> IO ()
draw window Game{..} = sequence_ [
  drawPlayer window player,
  drawDrops window player toDrop,
  drawBoard window player board]  
  
drawPlayer :: Window -> Player -> IO ()
drawPlayer window player = case player of
    Black -> setAvatarsClasses ["", desaturateClass]
    White -> setAvatarsClasses [desaturateClass , ""]
  where
    ids = [blackAvatarId, whiteAvatarId]
    setAvatarsClasses = zipWithM_ (#.) ((window?) <$> ids)

drawDrops :: Window -> Player -> [Piece] -> IO ()
drawDrops window player pieces = zipWithM_ (drawDrop window player) 
  (map count $ Piece <$> [Black, White] <*> [Chick, Elephant, Giraffe])
  dropsIds
  where
    count e = length $ filter (elem == e) pieces

drawDrop :: Window -> Player -> Int -> String -> IO ()
drawDrop window player count fieldId = sequence_ [
    field `setBackground` background # set UI.draggable canDrag,
    x2 #. clazz]
  where
    [field, x2] = map (window?) [fieldId, dropX2Id fieldId]
    background = if count > 0 then dropBackground fieldId else ""
    canDrag = count > 0 && dropOwner fieldId == player
    clazz = if count < 2 then hiddenClass else ""
      
drawBoard :: Window -> Player -> Board -> IO () 
drawBoard window player pieces = 
  zipWithM_ (drawField window player) (elems pieces) boardFieldsIds'
  
drawField :: Window -> Player -> Maybe Piece -> String -> IO ()
drawField window player piece id  = void $ 
  window ? id `setBackground` img 
    #. clazz 
    # set UI.draggable (pieceOwner == Just player)
    # set UI.droppable (pieceOwner /= Just player)
  where
    (pieceOwner, pieceKind) = (owner <$> piece, kind <$> piece)
    clazz = if pieceOwner == Just Black then 
      unwords [fieldClass, reversedClass] 
    else fieldClass
    img = maybe "" pieceToBackground pieceKind

