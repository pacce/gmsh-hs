{-# LANGUAGE FlexibleContexts #-}
module Codec.GMSH.Parse (mesh) where
import qualified Codec.GMSH.Parse.V1 as V1 (mesh)
import qualified Codec.GMSH.Parse.V2 as V2 (mesh)
import Codec.GMSH.Types (Mesh)
import Text.Parsec

mesh :: Stream s m Char => ParsecT s u m Mesh
mesh = choice [(try V1.mesh), (try  V2.mesh)]
