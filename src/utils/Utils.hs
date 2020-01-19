module Utils where

import Data.Bool
import qualified Data.Map as Map
import AbsLatte

-- Class 
type LLVMClassMap = Map.Map Ident LLVMClass
data LLVMClass = LLVMClass {
    className :: Ident,
    classFields :: [ClassPole],
    classMethods :: [ClassPole]
}

-- DFS Class ext
type LLVMClassVisited = Map.Map Ident Bool
type LLVMClassDefMap = Map.Map Ident TopDef

createClassMapFromTopDefs :: [TopDef] -> LLVMClassMap
createClassMapFromTopDefs topDefs =  classMapFromTopDefs topDefs classDefMap Map.empty Map.empty where classDefMap = foldr (\c -> \m -> case c of 
                                                                                                                    (ClassDef ident _) -> Map.insert ident c m
                                                                                                                    (ClassDefExt ident _ _) -> Map.insert ident c m
                                                                                                                    _ -> m
                                                                                                                ) Map.empty topDefs
                                                        


classMapFromTopDefs :: [TopDef] -> LLVMClassDefMap -> LLVMClassMap -> LLVMClassVisited -> LLVMClassMap
classMapFromTopDefs [] _  m _ = m
classMapFromTopDefs ((ClassDef ident classPoles):xs) cM m cV= case (Map.lookup ident) cV of
                                                                        Just _ -> classMapFromTopDefs xs cM m cV
                                                                        Nothing -> let newM = Map.insert ident (llvmClassFromPoles classPoles) m in
                                                                                        let newCV = Map.insert ident True cV in
                                                                                            classMapFromTopDefs xs cM newM newCV 
classMapFromTopDefs ((ClassDefExt ident identBase classPoles):xs) cM m cV = error ""
classMapFromTopDefs _ _ m _ = m

llvmClassFromPoles :: Ident -> [ClassPole] -> LLVMClass
llvmClassFromPoles ident classPoles = LLVMClass {
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