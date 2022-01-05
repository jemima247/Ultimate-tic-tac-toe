module XORules where

import Data.Array
import Data.Int
import Data.Maybe
import Data.Ring
import Effect.Console
import Prelude
import Types

import Ansi.Output (background)
import Board (Board(..), Cell(..))
import Data.Either (Either(..))
import Data.Newtype (overF)
import Effect (Effect)



same :: Maybe Cell -> Maybe Cell -> Maybe Cell -> Maybe Boolean
same c1 c2 c3 = case c1 of
    Just x -> case c2 of 
      Just y -> case c3 of 
        Just z -> Just ((cellValue x) == (cellValue y) && (cellValue y) == (cellValue z))
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing

ifWon :: Maybe Board -> Maybe Cell
ifWon b = case b of 
    Just (BWin x) -> Just x
    _ -> Nothing

sameB :: Maybe Board -> Maybe Board -> Maybe Board -> Maybe Boolean
sameB b1 b2 b3 = same (ifWon b1) (ifWon b2) (ifWon b3)


cellValue :: Cell -> String
cellValue c = case c of
  CX _ -> "X"
  CO _ -> "O"
  CEmpty _ -> " "


cellIndex :: Cell -> Int
cellIndex c = case c of
    CX a -> (a - 1)
    CO a -> (a - 1)
    CEmpty a -> (a - 1)



one :: Board -> Maybe Boolean
one b = case b of
    BSingle xs -> do
        first <- same (index xs 0) (index xs 1) (index xs 2)
        second <- same (index xs 0) (index xs 3) (index xs 6)
        third <- same (index xs 0) (index xs 4) (index xs 8)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 0) (index xs 1) (index xs 2)
        second <- sameB (index xs 0) (index xs 3) (index xs 6)
        third <- sameB (index xs 0) (index xs 4) (index xs 8)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    _ -> Nothing

two :: Board -> Maybe Boolean
two b = case b of
    BSingle xs -> do
        first <- same (index xs 1) (index xs 0) (index xs 2)
        second <- same (index xs 1) (index xs 4) (index xs 7)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 1) (index xs 0) (index xs 2)
        second <- sameB (index xs 1) (index xs 4) (index xs 7)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    _ -> Nothing

three :: Board -> Maybe Boolean
three b = case b of
    BSingle xs -> do
        first <- same (index xs 2) (index xs 1) (index xs 0)
        second <- same (index xs 2) (index xs 4) (index xs 6)
        third <- same (index xs 2) (index xs 5) (index xs 8)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 2) (index xs 1) (index xs 0)
        second <- sameB (index xs 2) (index xs 4) (index xs 6)
        third <- sameB (index xs 2) (index xs 5) (index xs 8)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    _ -> Nothing

four :: Board -> Maybe Boolean
four b = case b of
    BSingle xs -> do
        first <- same (index xs 3) (index xs 0) (index xs 6)
        second <- same (index xs 3) (index xs 4) (index xs 5)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 3) (index xs 0) (index xs 6)
        second <- sameB (index xs 3) (index xs 4) (index xs 5)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    _ -> Nothing

five :: Board -> Maybe Boolean
five b = case b of
    BSingle xs -> do
        first <- same (index xs 4) (index xs 0) (index xs 8)
        second <- same (index xs 4) (index xs 2) (index xs 6)
        third <- same (index xs 4) (index xs 1) (index xs 7)
        fouth <- same (index xs 4) (index xs 3) (index xs 5)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> case fouth of 
                        true -> Just true
                        _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 4) (index xs 0) (index xs 8)
        second <- sameB (index xs 4) (index xs 2) (index xs 6)
        third <- sameB (index xs 4) (index xs 1) (index xs 7)
        fouth <- sameB (index xs 4) (index xs 3) (index xs 5)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> case fouth of 
                        true -> Just true
                        _ -> Just false
    _ -> Nothing


six :: Board -> Maybe Boolean
six b = case b of
    BSingle xs -> do
        first <- same (index xs 5) (index xs 8) (index xs 2)
        second <- same (index xs 5) (index xs 4) (index xs 3)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 5) (index xs 8) (index xs 2)
        second <- sameB (index xs 5) (index xs 4) (index xs 3)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    _ -> Nothing

seven :: Board -> Maybe Boolean
seven b = case b of
    BSingle xs -> do
        first <- same (index xs 6) (index xs 3) (index xs 0)
        second <- same (index xs 7) (index xs 8) (index xs 6)
        third <- same (index xs 2) (index xs 6) (index xs 4)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 6) (index xs 3) (index xs 0)
        second <- sameB (index xs 7) (index xs 8) (index xs 6)
        third <- sameB (index xs 2) (index xs 6) (index xs 4)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    _ -> Nothing

eight :: Board -> Maybe Boolean
eight b = case b of
    BSingle xs -> do
        first <- same (index xs 7) (index xs 8) (index xs 6)
        second <- same (index xs 7) (index xs 4) (index xs 1)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 7) (index xs 8) (index xs 6)
        second <- sameB (index xs 7) (index xs 4) (index xs 1)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> Just false
    _ -> Nothing

nine :: Board -> Maybe Boolean
nine b = case b of
    BSingle xs -> do
        first <- same (index xs 8) (index xs 2) (index xs 5)
        second <- same (index xs 4) (index xs 8) (index xs 0)
        third <- same (index xs 8) (index xs 6) (index xs 7)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    BWhole xs -> do
        first <- sameB (index xs 8) (index xs 2) (index xs 5)
        second <- sameB (index xs 4) (index xs 8) (index xs 0)
        third <- sameB (index xs 8) (index xs 6) (index xs 7)
        case first of 
            true -> Just true
            _ -> case second of 
                true -> Just true
                _ -> case third of 
                    true -> Just true
                    _ -> Just false
    _ -> Nothing


check :: Board -> Cell -> Either String Board
check board cell = case board of 
    BSingle _ -> case cellIndex cell of 
        0 -> case one board of 
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "1something went wrong"
        1 -> case two board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "2something went wrong"
        2 -> case three board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "3something went wrong"
        3 -> case four board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "4something went wrong"
        4 -> case five board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "5 something went wrong"
        5 -> case six board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "6something went wrong"
        6 -> case seven board of    
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "7something went wrong"
        7 -> case eight board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "8something went wrong"
        8 -> case nine board of
            Just true -> Right (BWin cell)
            Just false -> Right board
            Nothing -> Left "9something went wrong"
        _ -> Left "something went wrong"
    _ -> Left "wrong board"



checkU :: Board -> Board -> Cell -> Either String Board
checkU bigBoard board cell = case bigBoard of 
    BWhole xs -> case check board cell of 
        Right (BWin x) -> 
            case elemIndex board xs of
                Just i -> case i of 
                    0 -> case one bigBoard of 
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "1something went wrong"
                    1 -> case two bigBoard of
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "2something went wrong"
                    2 -> case three bigBoard of
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "3something went wrong"
                    3 -> case four bigBoard of
                        Just true -> Left ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "4something went wrong"
                    4 -> case five bigBoard of
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "5 something went wrong"
                    5 -> case six bigBoard of
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "6something went wrong"
                    6 -> case seven bigBoard of    
                        Just true -> Left  ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "7something went wrong"
                    7 -> case eight bigBoard of
                        Just true -> Left ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "8something went wrong"
                    8 -> case nine bigBoard of
                        Just true -> Left ((cellValue cell) <> " wins")
                        Just false -> Right bigBoard
                        Nothing -> Left "9something went wrong"
                    _ -> Left "something went wrong in checkU"
                _ -> Left "coudn't get index"
        Right newBoard -> Right bigBoard
        Left x -> Left "something went wrong original from check"
    _ -> Left "wrong board"