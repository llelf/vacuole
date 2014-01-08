module Vacuole.View.Types (Colour(..),Kind(..),
                           Node(..),Link(..)) where


data Colour = Green | Blue | Red
              deriving Show

data Kind = Vanilla | ArrWords
          deriving (Eq,Show)


data Node = Node {
      size::Int, colour::Colour, kind::Kind,
      name::String, desc::String
} deriving Show


data Link = Link { from, to :: Int }

