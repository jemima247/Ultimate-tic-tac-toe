module Parser where

import Data.Array
import Data.Int
import Data.Maybe
import Effect.Console
import Effect (Effect)
import Prelude
import Types

import Control.Alt ((<|>))
import Board (Board(..), Cell(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Types (List, NonEmptyList(..))
import Data.Newtype (overF)
import Data.Show.Generic (genericShow)

import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodeUnits (noneOf, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (many1, sepBy, sepBy1)
import Text.Parsing.StringParser.Combinators as Comb
import XORules (cellIndex)

data CellP = Atom String | CList (List CellP)

derive instance eqCellP :: Eq CellP
derive instance genericCellP :: Generic CellP _
instance showCellP :: Show CellP where show x = genericShow x




cellParser' :: Parser CellP -> Parser CellP
cellParser' rec = parseList 
  where
    parseList = do
       NonEmptyList es <- Comb.between (string "(") (string ")") (rec `sepBy1` skipSpaces)
       pure $ CList (List.fromFoldable es)
    

cellParser :: Parser CellP
cellParser = Comb.fix cellParser'

asCell :: CellP -> Either String Cell
asCell (CList (Atom "CX" : Atom c1: Nil)) = do 
    case fromString c1 of
        Just x -> Right (CX x)
        Nothing -> Left "error in cx int"
asCell (CList (Atom "CO" : Atom c1: Nil)) = do 
    case fromString c1 of
        Just x -> Right (CO x)
        Nothing -> Left "error in co int"
asCell (CList (Atom "CEmpty" : Atom c1: Nil)) = do 
    case fromString c1 of
        Just x -> Right (CEmpty x)
        Nothing -> Left "error in cempty int"
asCell _ = Left "error in cell"


stringToCell :: String -> Either String Cell
stringToCell s = case (runParser cellParser s) of 
    Left err -> Left err.error
    Right c -> asCell c

play :: Board -> Array String ->  Effect Unit
play board inputs = plays board inputs
    where 
        plays :: Board -> Array String ->  Effect Unit
        plays bd xs = case uncons xs of 
            Just {head : x, tail : s} -> do
                logShow x
                case (stringToCell x) of 
                    Left err -> logShow ("first" <> err)
                    Right c -> do
                        case (positionBoard bd c 0 (cellIndex c)) of
                            Left e -> logShow e
                            Right bd' -> plays bd' s
            Nothing -> logShow "All inputs processed"
-- derive instance eqplays :: Eq plays
-- derive instance genericplays :: Generic plays _
-- instance showplays :: Show plays where show x = genericShow x
                                
                                
                                