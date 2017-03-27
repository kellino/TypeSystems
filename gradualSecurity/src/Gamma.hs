module Gamma where

import Syntax
import qualified Data.Map as M

initCtx :: Ctx
initCtx = Ctx { termenv = M.empty, gamma = Gamma (M.empty) }

extendGamma :: Ctx -> (String, GType) -> Ctx
extendGamma ctx (nm, gt) = 
    let (Gamma old) = gamma ctx
     in ctx { termenv = termenv ctx, gamma = Gamma (M.insert nm gt old) }

remove :: Ctx -> String -> Ctx
remove ctx nm = 
    let (Gamma old) = gamma ctx
     in ctx { termenv = termenv ctx, gamma = Gamma (M.delete nm old) }
