module Lib
    ( lexer
    ) where

import Text.Read

data Token = End
           | Quote String
           | Newline
           | Equals
           | LParen
           | RParen
           | Star
           | Slash
           | Plus
           | Dash
           | Identifier String
           | Integer Integer
           | Double Double
           | Type DataType
           deriving Show

data DataType = DataInt
              | DataString
              deriving Show

getTokens :: String -> [Token]
getTokens ""            = []
getTokens ('"':cs)      = Quote quoted : getTokens (drop 1 unquoted)
  where
    (quoted, unquoted) = span (/= '"') cs
getTokens (c:cs) | c `elem` " :" = getTokens cs
getTokens ('\n':cs)     = Newline : getTokens remainder
  where
    remainder = dropWhile (== '\n') cs
getTokens ('=':cs)      = Equals : getTokens cs
getTokens ('(':cs)      = LParen : getTokens cs
getTokens (')':cs)      = RParen : getTokens cs
getTokens ('*':cs)      = Star : getTokens cs
getTokens ('/':cs)      = Slash : getTokens cs
getTokens ('+':cs)      = Plus : getTokens cs
getTokens ('-':cs)      = Dash : getTokens cs
getTokens string@(c:cs) = getToken token : getTokens unconsumed
  where
    (token, unconsumed) = span (\c -> c `notElem` " :\n=()*/+-") string
    -- check for all single character tokens

getToken string = case (readMaybe string :: Maybe Integer) of
    Just integer -> Integer integer
    Nothing      -> case (readMaybe string :: Maybe Double) of
        Just double -> Double double
        Nothing     -> case string of
            "end"     -> End
            "Int"     -> Type DataInt
            "String"  -> Type DataString
            otherwise -> Identifier string

lexer :: String -> [Token]
lexer = getTokens
