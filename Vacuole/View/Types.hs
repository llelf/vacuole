module Vacuole.View.Types (Purpose(..),
                           Node(..),Link(..)) where


-- data Kind = Vanilla | Cons | ArrWords | Nowhere
--           deriving (Eq,Show,Read)


data Purpose
    = Vanilla String
    | EmptyList
    | Cons
    | ArrWords
    | Fun
    | Nowhere
      deriving (Read,Show,Eq)

data Node = Node Purpose String
            deriving Show


-- data Node = Node {
--       kind::Kind, name::String, desc::String
-- } deriving Show


data Link = Link { from, to :: Int }
            deriving Show
