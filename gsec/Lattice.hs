{-# LANGUAGE DeriveGeneric #-}

module Lattice where

import GHC.Generics (Generic)

class Ord a => Lattice a where
    top :: a
    bottom :: a
    (\/) :: a -> a -> a
    (/\) :: a -> a -> a
    flowsTo :: a -> a -> Bool

-- | slightly more interesting lattice
--                   H
--                 /  \
--                MA  MB
--                \   /
--                  L

data GLabel = L | MA | MB | H | Any deriving (Show, Eq, Ord, Generic)

-- there must be a better way than this!
-- this needs to be generated algorthimcally. To discuss
instance Lattice GLabel where
    top = H
    bottom = L
    -- joins
    L \/ L = L
    L \/ MA = MA
    L \/ MB = MB
    L \/ H = H
    L \/ Any = Any
    MA \/ L = MA
    MA \/ MA = MA
    MA \/ MB = H
    MA \/ H = H
    MA \/ Any = Any
    MB \/ L = MB
    MB \/ MA = H
    MB \/ MB = MB
    MB \/ H = H
    MB \/ Any = Any
    H \/ L = H
    H \/ MA = H
    H \/ MB = H
    H \/ H = H
    H \/ Any = H
    Any \/ L = Any
    Any \/ MA = Any
    Any \/ MB = Any
    Any \/ H = H
    Any \/ Any = Any
    -- meets
    L /\ L = L
    L /\ MA = L
    L /\ MB = L
    L /\ H = L
    L /\ Any = L
    MA /\ L = L
    MA /\ MA = MA
    MA /\ MB = L
    MA /\ H = MA
    MA /\ Any = Any
    MB /\ L = L
    MB /\ MA = L
    MB /\ MB = MB
    MB /\ H = MB
    MB /\ Any = Any
    H /\ L = L
    H /\ MA = MA
    H /\ MB = MB
    H /\ H = H
    H /\ Any = Any
    Any /\ L = L
    Any /\ MA = Any
    Any /\ MB = Any
    Any /\ H = Any
    Any /\ Any = Any
    -- permissible flows
    flowsTo L L = True
    flowsTo L MA = True
    flowsTo L MB = True
    flowsTo L H = True
    flowsTo L Any = True
    flowsTo MA L = False
    flowsTo MA MA = True
    flowsTo MA MB = False
    flowsTo MA H = True
    flowsTo MA Any = True
    flowsTo MB L = False
    flowsTo MB MA = False
    flowsTo MB MB = True
    flowsTo MB H = True
    flowsTo MB Any = True
    flowsTo H L = False
    flowsTo H MA = False
    flowsTo H MB = False
    flowsTo H H = True
    flowsTo H Any = True
    flowsTo Any L = True
    flowsTo Any MA = True
    flowsTo Any MB = True
    flowsTo Any H = True
    flowsTo Any Any = True
