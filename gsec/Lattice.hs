{-# LANGUAGE DeriveGeneric #-}

module Lattice where

import GHC.Generics (Generic)

class Ord a => Lattice a where
    top :: a
    bottom :: a
    join :: a -> a -> a
    meet :: a -> a -> a
    flowsTo :: a -> a -> Bool

-- slightly more interesting lattice
--                   H
--                 /  \
--                MA  MB
--                \   /
--                  L

data GLabel = L | MA | MB | H | Any deriving (Show, Ord, Generic)

instance Eq GLabel where
    L == L = True
    L == Any = True
    MA == MA = True
    MA == Any = True
    MB == MB = True
    MB == Any = True
    H == H = True
    H == Any = True
    _ == _ = False

instance Lattice GLabel where
    top = H
    bottom = L
    join x y = if x `flowsTo` y then y else x
    meet x y = if x `flowsTo` y then x else y
    -- low
    flowsTo L _ = True
    -- middle a
    flowsTo MA L = False
    flowsTo MA _ = True
    -- middle b
    flowsTo MB L = False
    flowsTo MB MA = False
    flowsTo MB _ = True
    -- high
    flowsTo H H = True
    flowsTo H Any = True
    flowsTo H _ = False
    flowsTo _ _ = True
