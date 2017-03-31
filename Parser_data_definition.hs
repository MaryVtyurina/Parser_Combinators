-- /Users/Maria/Library/Haskell/bin/ghcid "--command=ghci Parse_list.hs"     -- for ghcid
module Parser_data_definition where
import ParserCombinators

{-#LANGUAGE MultiParamTypeClasses, MonadComprehensions #-}

data Type = Arrow Type Type -- function
          | Apply Type Type -- Application
          | Var String      -- variable
          | Con String      -- constructor
          | Tuple [Type]    -- Tuple
          | List Type       -- List
          deriving (Show, Eq)

variable = identifier ["data"]

var = variable >>= return . Var

list = List <$> bracket (symbol "[") type0 (symbol "]")

tuple = do
            ts <- bracket (symbol "(") (type0 `sepby` symbol ",") (symbol ")")
            return $ f ts
            where
                f [t] = t
                f ts  = Tuple ts

constructor = do
                x  <- upper
                xs <- many alphanum
                token $ return $ x : xs

con = do
        x <- constructor
        return $ Con x

type0 :: Parser Type
type0 = type1 `chainr1` (symbol "->" >> return Arrow)

type1 = type2 `chainl1` return Apply

type2 = var +++ con +++ list +++ tuple

condecl = do
            x  <- constructor
            ts <- many type2
            return (x,ts)

datadecl = do
             _  <- symbol "data"
             x  <- constructor
             xs <- many variable
             _  <- symbol "="
             b  <- condecl `sepby` symbol "|"
             return (x,xs,b)

datadecls = first $ many_offside datadecl
