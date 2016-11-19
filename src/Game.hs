module Game where

data Game = Game { localPlayer :: Player
                 , otherPlayers :: [Player]
                 }

type Player = (Float, Float)
