module Codec.GMSH.Types (
        Coordinate,
        Element(..),
        ElementType(..),
        Format(..),
        Mesh(..),
        Node(..)
        ) where

data Mesh = Mesh Format [Node] [Element]
    deriving (Show)

data Format = Format {
    versionNumber   :: Float,
    fileType        :: Int,
    dataSize        :: Int
} deriving (Show)

type Coordinate = Float
type Index = Int

data Node = Node {
    nodeNumber  :: Index,
    x           :: Coordinate,
    y           :: Coordinate,
    z           :: Coordinate
} deriving (Show)

data Element = Element {
    elementNumber   :: Index,
    elementTags     :: [Index],
    nodeList        :: ElementType
} deriving (Show)

data ElementType
    = Line Index Index
    | Line3 Index Index Index
    | Line4 Index Index Index Index
    | Triangle Index Index Index
    | Triangle6 Index Index Index Index Index Index
    | Triangle9 Index Index Index Index Index Index Index Index Index
    | Triangle10 Index Index Index Index Index Index Index Index Index Index
    | Triangle12 Index Index Index Index Index Index Index Index Index Index Index Index
    | Triangle15 Index Index Index Index Index Index Index Index Index Index Index Index Index Index Index
    | Quadrangle Index Index Index Index
    | Quadrangle8 Index Index Index Index Index Index Index Index
    | Quadrangle9 Index Index Index Index Index Index Index Index Index
    | Tetrahedron Index Index Index Index
    | Tetrahedron10 Index Index Index Index Index Index Index Index Index Index
    | Hexahedron Index Index Index Index Index Index Index Index
    deriving (Show)
