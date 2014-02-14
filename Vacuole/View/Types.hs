module Vacuole.View.Types (GraphView,Purpose(..),
                           Node(..),Link(..)) where


type GraphView = ([Node],[Link])

-- data Kind = Vanilla | Cons | ArrWords | Nowhere
--           deriving (Eq,Show,Read)


data Purpose
    = Vanilla String
    | EmptyList
    | Cons
    | ArrWords
    | ArrPtrs
    | Fun
    | Nowhere
      deriving (Read,Show,Eq)

data Node = Node Purpose String
            deriving (Read,Show)


-- data Node = Node {
--       kind::Kind, name::String, desc::String
-- } deriving Show


data Link = Link { from, to :: Int }
            deriving (Read,Ord,Eq,Show)
