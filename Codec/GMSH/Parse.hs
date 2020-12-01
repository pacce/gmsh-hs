{-# LANGUAGE FlexibleContexts #-}

module Codec.GMSH.Parse () where
import qualified Codec.GMSH.Parse.V2 as V2 (mesh)
import Codec.GMSH.Types (Mesh)
import Text.Parsec

mesh :: Stream s m Char => ParsecT s u m Mesh
mesh = V2.mesh
