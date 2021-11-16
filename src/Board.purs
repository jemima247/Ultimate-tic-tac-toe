module Board where

import Prelude
import Data.Generic.Rep (class Generic)
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
    | BEmpty

derive instance eqBoard :: Eq Board
derive instance genericBoard :: Generic Board _
instance showBoard :: Show Board where show x = genericShow x

    
     

    