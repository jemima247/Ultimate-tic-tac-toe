module Board where

import Data.Array
import Data.Maybe
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int.Bits (xor)
import Data.Newtype (overF)
import Data.Show.Generic (genericShow)


data Cell
    = CX Int
    | CO Int
    | CEmpty Int

derive instance eqCell :: Eq Cell
derive instance genericCell :: Generic Cell _
instance showExp :: Show Cell where show x = genericShow x

data Board 
    = BSingle (Array Cell) -- if it has only one cell then it is a solved board
    -- | BSingle Cell
    | BWhole (Array Board)
    | BEmpty (Array Cell)
    -- | BEmpty (Array Board)

derive instance eqBoard :: Eq Board
derive instance genericBoard :: Generic Board _
-- instance showBoard :: Show Board where show x = genericShow x

--log "|\t|\t|\t|\n|\t|\t|\t|\n|\t|\t|\t|"

showBoard :: Board -> String
showBoard board = case board of 
    BSingle a -> "__________________________________\n" <> showBoard' a 1
        where
            showBoard' :: Array Cell -> Int -> String
            showBoard' xs a =  
                if a == 3 then 
                    case uncons xs of
                        Just {head : x, tail : s} -> case x of
                            CEmpty _ -> "|" <> show x <> "|\n" <> "__________________________________\n" <> showBoard' s 1
                            _ -> "|  " <> show x <> "  |\n" <> "__________________________________\n" <> showBoard' s 1
                        _ -> ""
                else
                    case uncons xs of
                        Just {head : x, tail : s} -> case x of
                            CEmpty _ -> "|" <> show x <> "" <> showBoard' s (a + 1)
                            _ -> "|  " <> show x <> "  " <> showBoard' s (a + 1)
                        _ -> ""
    BWhole arr -> "not done yet"
    BEmpty arr -> "not done yet"







    
     

    