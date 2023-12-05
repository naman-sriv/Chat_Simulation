module Types (
  User (..),
  Msg (..)
)where

newtype User = User {
    name :: String
    } deriving (Show, Eq)

data Msg = Msg {
    sender :: String, 
    receiver :: String, 
    message :: String} deriving (Show)