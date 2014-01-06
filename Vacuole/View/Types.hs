module Vacuole.View.Types (Colour(..),Node(..),Link(..)) where


data Colour = Green | Blue | Red
              deriving Show


data Node = Node {
      size::Int, colour::Colour, name::String, desc::String
} deriving Show


data Link = Link { from, to :: Int }

