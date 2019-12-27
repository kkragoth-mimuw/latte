module Frontend.Typechecker.Environment where

import qualified Data.Map as Map

import AbsLatte

data TCEnv = TCEnv {  typesMap                  :: Map.Map Ident (Type, Integer)
                    , level                     :: Integer
                    , currentFunctionReturnType :: Maybe Type
                    }

initTCEnv = TCEnv {   typesMap = Map.empty
                    , level = 0
                    , currentFunctionReturnType = Nothing
                    }
