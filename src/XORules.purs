module XORules where

import Data.Array
import Data.Int
import Data.Maybe
import Prelude
import Types

import Board (Board(..), Cell(..))
import Data.Either (Either(..))
import Data.Newtype (overF)



same :: Maybe Cell -> Maybe Cell -> Maybe Cell -> Maybe Boolean
same c1 c2 c3 = case c1 of
    Just x -> case c2 of 
      Just y -> case c3 of 
        Just z -> Just (x == y && y == z)
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing


cellValue :: Cell -> String
cellValue c = case c of
  CX _ -> "X"
  CO _ -> "O"
  CEmpty _ -> " "


cellIndex :: Cell -> Int
cellIndex c = case c of
    CX a -> a
    CO a -> a
    CEmpty a -> a


one :: Board -> Maybe Boolean
one b = case b of
    BSingle xs -> do
        first <- same (index xs 1) (index xs 2) (index xs 3)
        second <- same (index xs 1) (index xs 4) (index xs 7)
        third <- same (index xs 1) (index xs 5) (index xs 9)
        case first of 
            true -> case second of 
                true -> case third of 
                    true -> Just true
                    _ -> Just false
                _ -> Just false
            _ -> Just false
    _ -> Nothing

two :: Board -> Maybe Boolean
two b = case b of
    BSingle xs -> do
        first <- same (index xs 2) (index xs 1) (index xs 3)
        second <- same (index xs 2) (index xs 5) (index xs 8)
        case first of 
            true -> case second of 
                true -> Just true
                _ -> Just false
            _ -> Just false
    _ -> Nothing

three :: Board -> Maybe Boolean
three b = case b of
    BSingle xs -> do
        first <- same (index xs 3) (index xs 2) (index xs 1)
        second <- same (index xs 3) (index xs 5) (index xs 7)
        third <- same (index xs 3) (index xs 6) (index xs 9)
        case first of 
            true -> case second of 
                true -> case third of 
                    true -> Just true
                    _ -> Just false
                _ -> Just false
            _ -> Just false
    _ -> Nothing

four :: Board -> Maybe Boolean
four b = case b of
    BSingle xs -> do
        first <- same (index xs 4) (index xs 1) (index xs 7)
        second <- same (index xs 4) (index xs 5) (index xs 6)
        case first of 
            true -> case second of 
                true -> Just true
                _ -> Just false
            _ -> Just false
    _ -> Nothing

five :: Board -> Maybe Boolean
five b = case b of
    BSingle xs -> do
        first <- same (index xs 5) (index xs 1) (index xs 9)
        second <- same (index xs 5) (index xs 3) (index xs 7)
        third <- same (index xs 5) (index xs 2) (index xs 8)
        fouth <- same (index xs 5) (index xs 4) (index xs 6)
        case first of 
            true -> case second of 
                true -> case third of 
                    true -> case fouth of
                        true -> Just true
                        _ -> Just false
                    _ -> Just false
                _ -> Just false
            _ -> Just false
    _ -> Nothing

six :: Board -> Maybe Boolean
six b = case b of
    BSingle xs -> do
        first <- same (index xs 6) (index xs 9) (index xs 3)
        second <- same (index xs 6) (index xs 5) (index xs 4)
        case first of 
            true -> case second of 
                true -> Just true
                _ -> Just false
            _ -> Just false
    _ -> Nothing

seven :: Board -> Maybe Boolean
seven b = case b of
    BSingle xs -> do
        first <- same (index xs 7) (index xs 4) (index xs 1)
        second <- same (index xs 8) (index xs 9) (index xs 7)
        third <- same (index xs 3) (index xs 7) (index xs 5)
        case first of 
            true -> case second of 
                true -> case third of 
                    true -> Just true
                    _ -> Just false
                _ -> Just false
            _ -> Just false
    _ -> Nothing

eight :: Board -> Maybe Boolean
eight b = case b of
    BSingle xs -> do
        first <- same (index xs 8) (index xs 9) (index xs 7)
        second <- same (index xs 8) (index xs 5) (index xs 2)
        case first of 
            true -> case second of 
                true -> Just true
                _ -> Just false
            _ -> Just false
    _ -> Nothing

nine :: Board -> Maybe Boolean
nine b = case b of
    BSingle xs -> do
        first <- same (index xs 9) (index xs 3) (index xs 6)
        second <- same (index xs 5) (index xs 9) (index xs 1)
        third <- same (index xs 9) (index xs 7) (index xs 8)
        case first of 
            true -> case second of 
                true -> case third of 
                    true -> Just true
                    _ -> Just false
                _ -> Just false
            _ -> Just false
    _ -> Nothing


check :: Board -> Cell -> Either String Board
check board cell = case board of 
    BSingle xs -> case cellIndex cell of 
        1 -> case one board of 
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        2 -> case two board of
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        3 -> case three board of
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        4 -> case four board of
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        5 -> case five board of
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        6 -> case six board of
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        7 -> case seven board of    
            Just true -> Left (cellValue cell)
            Just false -> Right board
            Nothing -> Left "something went wrong"
        8 -> case eight board of
            Just true -> Left ((cellValue cell) <> " wins")
            Just false -> Right board
            Nothing -> Left "something went wrong"
        9 -> case nine board of
            Just true -> Left ((cellValue cell) <> " wins")
            Just false -> Right board
            Nothing -> Left "something went wrong"
        _ -> Left "something went wrong"
    _ -> Left "wrong board"
