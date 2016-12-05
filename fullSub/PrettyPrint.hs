module PrettyPrint where

import Syntax

import qualified Data.Text.IO as T
import qualified Data.Text as T

remComments :: T.Text -> T.Text
remComments = T.strip . T.takeWhile (/= '#') 
