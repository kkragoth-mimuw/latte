module Frontend.Typechecker.Environment where

import qualified Data.Map as Map

import AbsLatte

data TCEnv = TCEnv {  typesMap                  :: Map.Map Ident (Type, Integer)
                    , level                     :: Integer
                    , currentFunctionReturnType :: Maybe Type
                    }

initTCEnv = TCEnv {   typesMap = Map.fromList [
    (Ident "printInt", ((Fun Void ([Int])), -1)),
    (Ident "printString", ((Fun Void ([Str])), -1)),
    (Ident "error", ((Fun Void []), -1)),
    (Ident "readInt", ((Fun Int []), -1)),
    (Ident "readString", ((Fun Str []), -1))
    ]
                    , level = 0
                    , currentFunctionReturnType = Nothing
                    }
