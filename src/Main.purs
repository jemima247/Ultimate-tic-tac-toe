module Main where

import Effect.Console
import Prelude
import Types
import XORules

import Board (Board(..), Cell(..))
import Parser
import Data.Either 
import Effect (Effect)


inputs :: Array String
inputs = [ "(CX 5)", "(CO 1)", "(CX 3)", "(CO 7)", "(CX 6)", "(CO 4)" ]
--now create a parser functio to do what im trying to do at the bottom
-- first start with an empty board
-- parse the input do the first string
-- check if board was won and show result
-- if not parse next string
-- check again
-- if not parse next string
-- make this a recursive function that has exit for full, won or error board
main :: Effect Unit
main = do
  let board = BSingle [CEmpty 1, CEmpty 2, CEmpty 3, CEmpty 4, CEmpty 5, CEmpty 6, CEmpty 7, CEmpty 8, CEmpty 9]
  log "Hello there this is ultimate tic tac toe!!!"
  logShow board
  logShow inputs
  play board inputs
  -- time "start time"
  -- timeLog "start time"
  -- clear
