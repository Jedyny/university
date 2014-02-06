module Identifiers where

import Engine

{-- Paths --}

images :: String
images = "/static/img/"

{-- Names of classes --}

desaturateClass :: String
desaturateClass = "desaturate"

fieldClass :: String
fieldClass = "field"

hiddenClass :: String
hiddenClass = "hidden"

reversedClass :: String
reversedClass = "reversed"

{-- Ids of layout elements -}

blackAvatarId :: String
blackAvatarId = "black_avatar"

blackChickDropId :: String
blackChickDropId = "black_chick_drop"

blackElephantDropId :: String
blackElephantDropId = "black_elephant_drop"

blackGiraffeDropId :: String
blackGiraffeDropId = "black_giraffe_drop"

whiteAvatarId :: String
whiteAvatarId = "white_avatar"

whiteChickDropId :: String
whiteChickDropId = "white_chick_drop"

whiteElephantDropId :: String
whiteElephantDropId = "white_elephant_drop"

whiteGiraffeDropId :: String
whiteGiraffeDropId = "white_giraffe_drop"

dropsIds :: [String] 
dropsIds = [blackChickDropId, blackElephantDropId, blackGiraffeDropId,
  whiteChickDropId, whiteElephantDropId, whiteGiraffeDropId]

drop2XsIds :: [String]
drop2XsIds = map dropX2Id dropsIds

boardFieldsIds :: [String]
boardFieldsIds = ["field0", "field1", "field2", "field3", "field4", 
  "field5", "field6", "field7", "field8", "field9", "field10",
  "field11"]
  
boardFieldsIds' :: [String]
boardFieldsIds' = ["field0", "field3", "field6", "field9", "field1", 
  "field4", "field7", "field10", "field2", "field5", "field8",
  "field11"]  

newGameDialogId :: String
newGameDialogId = "dialog"

newGameButtonId :: String
newGameButtonId = "new_game"

newGameInfoId :: String
newGameInfoId = "new_game_info"

simbaWinsId :: String
simbaWinsId = "simba_wins"

scarWinsId :: String
scarWinsId = "scar_wins"

{-- Utils --}

dropX2Id :: String -> String
dropX2Id id
  | id == blackChickDropId = "black_chick_drop_x2"
  | id == blackElephantDropId = "black_elephant_drop_x2"
  | id == blackGiraffeDropId = "black_giraffe_drop_x2"
  | id == whiteChickDropId = "white_chick_drop_x2"
  | id == whiteElephantDropId = "white_elephant_drop_x2"
  | id == whiteGiraffeDropId = "white_giraffe_drop_x2"

dropBackground :: String -> String
dropBackground id
  | id == blackChickDropId = images ++ "chick.png"
  | id == blackElephantDropId = images ++ "elephant.png"
  | id == blackGiraffeDropId = images ++ "giraffe.png"
  | id == whiteChickDropId = images ++ "chick.png"
  | id == whiteElephantDropId = images ++ "elephant.png"
  | id == whiteGiraffeDropId = images ++ "giraffe.png"
  
dropOwner :: String -> Player
dropOwner id
  | id == blackChickDropId = Black
  | id == blackElephantDropId = Black
  | id == blackGiraffeDropId = Black
  | id == whiteChickDropId = White
  | id == whiteElephantDropId = White
  | id == whiteGiraffeDropId = White

pieceToBackground :: PieceKind -> String
pieceToBackground Chick = images ++ "chick.png"
pieceToBackground Hen = images ++ "hen.png"
pieceToBackground Elephant = images ++ "elephant.png"
pieceToBackground Giraffe = images ++ "giraffe.png"
pieceToBackground Lion = images ++ "lion.png"     
