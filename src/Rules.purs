module Rules where

import Data.Array
import Data.Eq
import Data.Int
import Data.Map
import Data.Ord
import Effect.Console
import Prelude
import XORules

import Board (Board(..), Cell(..), showBoard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple1)
import Effect (Effect)
import Types (positionBoard)

type BoardInt = Int

-- first in ultimate we have that a smaller board is returned
start :: Board -> (Array Cell) -> Effect Unit
start board inputs = startU board inputs 4
    where
       startU :: Board -> (Array Cell) -> BoardInt -> Effect Unit
       startU board inputs st = case board of 
            (BWhole xs) -> case index xs st of 
                Just a -> do
                    log ("Starting board at index " <> show st)
                    log $ showBoard a
                    case uncons inputs of
                        Just {head : x, tail : s} -> do
                            log ("Input: " <> show x)
                            case (positionBoard board x st (cellIndex x)) of 
                                Left err -> logShow err
                                Right board' -> do
                                    logShow "full board:\n"
                                    log $ showBoard board'
                                    case board' of 
                                        BWhole ar -> case index ar st of 
                                            Just a -> do
                                                log $ showBoard a
                                                case checkU board' a x of
                                                    Left e -> logShow e
                                                    Right b -> startU b s (cellIndex x)
                                            Nothing -> logShow "no board at index"
                                        _ -> logShow "so a whole board"
                        Nothing -> logShow "no inputs"       
                Nothing -> logShow "no board at index2"
            _ -> logShow "wrong board"
                                                 








-- case (positionBoard bd x 0 (cellIndex x)) of
--                     Left e -> logShow e
--                     Right bd' -> do
--                         log $ showBoard bd'
--                         case check bd' x of
--                             Left e -> logShow e
--                             Right bd'' -> plays bd'' s
                

