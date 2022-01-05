module Types where

import Data.Array
import Data.Int
import Data.Maybe
import Prelude

import Board (Board(..), Cell(..))
import Data.Either (Either(..))

-- import Prim.Boolean 


type Step = Board -> Either String Board
type CellNum = Int
type BoardNum = Int



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
                        (BSingle v) ->  
                            case fullBoard v of
                                true -> do
                                    full' s (n+1)
                                false -> false
                        _ -> false
                Nothing -> n == 9

positionBoard :: Board -> Cell -> BoardNum -> CellNum-> Either String Board
positionBoard board cell bigIndex indexC = do 
    case board of 
        BEmpty xs -> do
            case updateAt indexC cell xs of 
                Nothing -> Left "Invalid index"
                Just x -> Right (BSingle x)
        BSingle xs -> 
            case fullBoard xs of 
                true -> Left "Board is full"
                false -> do 
                    case updateAt indexC cell xs of 
                        Nothing -> Left "Invalid index"
                        Just x -> Right (BSingle x)
        BWhole ys ->
            case fullBigBoard ys of 
                true -> Left "Board is full, No one won?"
                false -> do 
                    case (index ys bigIndex) of 
                        Just x -> do
                            case x of
                                BSingle y -> 
                                    case fullBoard y of 
                                        true -> Left "Board is full"
                                        false -> do 
                                            case updateAt indexC cell y of 
                                                Nothing -> Left "Invalid index"
                                                Just xy -> do
                                                    case updateAt bigIndex (BSingle xy) ys of
                                                        Nothing -> Left "Invalid index"
                                                        Just a -> Right (BWhole a)
                                BEmpty xs -> do
                                    case updateAt indexC cell xs of 
                                        Nothing -> Left "Invalid index"
                                        Just x -> Right (BSingle x)
                                _ -> Left "Invalid board arrangement" --try to do for an empty board
                        Nothing -> Left "Not sure why you got here"
        BWin _ -> Left "Already won"
                

                          
                    