module Vacuole.View.Types (Kind(..),
                           Node(..),Link(..)) where


data Kind = Vanilla | Cons | ArrWords | Nowhere
          deriving (Eq,Show,Read)




data Node = Node {
      kind::Kind, name::String, desc::String
} deriving Show


data Link = Link { from, to :: Int }

