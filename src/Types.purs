module Types where


import Data.Array
import Data.Int
import Prelude

import Board (Board(..))
import Data.Either (Either)
import Prim.Boolean (False)



type CellNum = Int

fullBoard :: Array Cell -> Boolean
fullBoard b = full' b 0
    where
        full' :: Array Cell -> Int -> Boolean
        full' xs n = do
            case uncons xs of 
                Just {head: x, tail: s} ->
                    case x of
                        CX _ -> do
                            full' s (n+1)
                        CO _ -> do
                            full' s (n+1)
                        _ -> do 
                            full' s (n)
                Nothing -> n == 9

fullBigBoard :: Array Board -> Boolean
fullBigBoard b = full' b 0
    where
        full' :: Array Board -> Int -> Boolean
        full' xs n = do
            case uncons xs of 
                Just {head: x, tail: s} ->
                    case x of
                        BSingle v ->  
                            case fullBoard v of
                                True -> do
                                    full' s (n+1)
                                False -> False
                        _ -> False
                Nothing -> n == 9

positionBoard :: Board -> Cell -> BoardNum -> CellNum-> Either String Board
positionBoard board cell bigIndex index = do 
    case board of 
        BEmpty -> _
        BSingle xs -> 
            case fullBoard xs of 
                True -> Left "Board is full"
                False -> do 
                    case updateAt index cell xs of 
                        Nothing -> Left "Invalid index"
                        Just x -> Right BSingle x
        BWhole xs ->
            case fullBigBoard xs of 
                True -> Left "Board is full, No one won?"
                False -> do 
                    case index xs bigIndex of 
                        Just x -> do
                          case updateAt index cell x of 
                            Nothing -> Left "Invalid index" -- find a way to check the board that was chosen out of th emain board like in the top
                    -- case updateAt index cell bigIndex cell xs of 
                    --     Nothing -> Left "Invalid index"
                    --     Just x -> Right BWhole x