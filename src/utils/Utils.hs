module Utils where

import Data.Bool
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import AbsLatte

-- Class 
type ClassMap = Map.Map Ident Class
data Class = Class {
    className :: Ident,
    classFields :: [ClassPole],
    classMethods :: [ClassPole]
}

-- DFS Class ext
type ClassDefMap = Map.Map Ident TopDef

createClassMapFromTopDefs :: [TopDef] -> ClassMap
createClassMapFromTopDefs topDefs =  classMapFromTopDefs topDefs classDefMap Map.empty where classDefMap = foldr (\c -> \m -> case c of 
                                                                                                                    (ClassDef ident _) -> Map.insert ident c m
                                                                                                                    (ClassDefExt ident _ _) -> Map.insert ident c m
                                                                                                                    _ -> m
                                                                                                                ) Map.empty topDefs
                                                        


classMapFromTopDefs :: [TopDef] -> ClassDefMap -> ClassMap -> ClassMap
classMapFromTopDefs [] _  m = m
classMapFromTopDefs ((ClassDef ident classPoles):xs) cM m= case (Map.lookup ident) m of
                                                                        Just _ -> classMapFromTopDefs xs cM m
                                                                        Nothing -> let newM = Map.insert ident (classFromPoles ident classPoles) m in
                                                                                        classMapFromTopDefs xs cM newM
classMapFromTopDefs ((ClassDefExt ident identBase classPoles):xs) cM m = case (Map.lookup ident) m of
                                                                            Just _ -> classMapFromTopDefs xs cM m
                                                                            Nothing -> case (Map.lookup identBase) m of
                                                                                Just baseC -> let newM = Map.insert ident (extendClass baseC (classFromPoles ident classPoles)) m in
                                                                                    classMapFromTopDefs xs cM newM
                                                                                Nothing -> let mWithBase = classMapFromTopDefs [(fromJust $ Map.lookup identBase cM) ] in
                                                                                                let baseC = fromJust $ Map.lookup identBase m in
                                                                                                    let newM = Map.insert ident (extendClass baseC (classFromPoles ident classPoles)) m in
                                                                                                        classMapFromTopDefs xs cM newM
classMapFromTopDefs _ _ m = m

classFromPoles :: Ident -> [ClassPole] -> Class
classFromPoles ident classPoles = Class {
    className = ident,
    classFields = ( filter (\cp -> case cp of 
                                        (ClassFieldDef _ _) -> True 
                                        _ -> False
                            ) classPoles 
                    ),
    classMethods = ( filter (\cp -> case cp of 
                                        (ClassMethodDef _ _ _ _) -> True 
                                        _ -> False
                            ) classPoles 
                    )
}

extendClass :: Class -> Class -> Class
extendClass baseClass derivedClass = Class {
    className = className derivedClass,
    classFields = (classFields baseClass) ++ (classFields derivedClass),
    classMethods = (classMethods baseClass) ++ (classMethods derivedClass)
}