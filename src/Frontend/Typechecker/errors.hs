module Frontend.Typechecker.Errors where

import Data.List
import Text.Printf

import AbsLatte
import PrintLatte

import Frontend.Typechecker.Utils

data TypecheckError = TCInvalidTypeExpectedType Type Type
                    | TCInvalidTypeExpectedTypes Type [Type]
                    | TCInvalidFunctionAppTypes [Type] [Type]
                    | TCUndeclaredVariable Ident
                    | TCNotLValue
                    | TCRedeclaration Ident
                    | TCDebug String

instance Show TypecheckError where
    show (TCInvalidTypeExpectedType type' allowedType)   = printf "Invalid type: %s. Expected: %s" (prettyShowType type') (prettyShowType allowedType)
    show (TCInvalidTypeExpectedTypes type' allowedTypes) = printf "Invalid type: %s. Expected %s" (prettyShowType type') (intercalate " or " (map prettyShowType allowedTypes))
    show (TCRedeclaration (Ident ident))                 = printf "Tried to redeclare: %s" (show ident)
    show TCNotLValue                                     = "Not lvalue"
    show (TCDebug str)                                   = printf "%s" (show str)
    show _ = ""

data TypecheckErrorWithLogging = TypecheckErrorWithLogging TypecheckError Integer [String] deriving (Show)


defaultLevelOfLogging = 7

initTypecheckError :: TypecheckError -> TypecheckErrorWithLogging
initTypecheckError error = TypecheckErrorWithLogging error defaultLevelOfLogging []


appendLogToTypecheckError :: (Print a) => TypecheckErrorWithLogging -> a -> TypecheckErrorWithLogging
appendLogToTypecheckError typecheckErrorWithLogging@(TypecheckErrorWithLogging error loggingLevel msg) location | loggingLevel <= 0 = typecheckErrorWithLogging
appendLogToTypecheckError                           (TypecheckErrorWithLogging error loggingLevel msg) location = TypecheckErrorWithLogging error (loggingLevel - 1) (msg ++ [printTree location])


pprintTypecheckerErrorMsg :: TypecheckErrorWithLogging -> IO ()
pprintTypecheckerErrorMsg wholeMsg@(TypecheckErrorWithLogging error _ stack) = do
    putStrLn ("Encountered error " ++ show error)
    mapM_ (\line -> putStrLn ("\nFound in:\n " ++  trim line))  stack


prettyShowType :: Type -> String
prettyShowType type_ = case type_ of
    Int   -> "int"
    Str   -> "string"
    Bool  -> "bool"
    Void  -> "void"
