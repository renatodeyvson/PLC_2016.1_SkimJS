module Value (Value (..)) where

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
  	| Func String [String]
    | List [Value]
    | Nil

--
-- Pretty Printer
--

instance Show Value where
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show (Func str args) = "function " ++ str ++ "(" ++ (showStringListContents args) ++ ")"
  show (List []) = "[]"
  show (List [a]) = show a
  show (List (a:xs)) = (show a)++","++(show (List xs))
  show Nil = "undefined"

-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

showStringListContents :: [String] -> String
showStringListContents [] = ""
showStringListContents (a:as) = show a ++ ", " ++ (showStringListContents as)
