-------------------------------------------------------------------
---------------------------Example---------------------------------
-------------------------------------------------------------------
-- /Users/Maria/Library/Haskell/bin/ghcid "--command=ghci Parse_list.hs"-- for ghcid

import Parse_list
import Control.Applicative hiding (many)

{-#LANGUAGE MultiParamTypeClasses, MonadComprehensions #-}

data Type = Arrow Type Type -- function
          | Apply Type Type -- Application
          | Var String      -- variable
          | Con String      -- constructor
          | Tuple [Type]    -- Tuple
          | List Type       -- List
          deriving (Show)

-- parses non empty sequences of items separated by operators that associate to the right, rather than to the left
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = rec <|> p where
        rec = do
                    x <- p
                    f <- op
                    y <- p `chainr1` op
                    return $ f x y


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest where
    rest x = one x <|> return x
    one x = do
        f <- op
        y <- p
        rest (f x y)

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

datadecls = many_offside datadecl

parse p s = unS (unR p (1,1)) ((1,1), s)

main = do
    s <- readFile "test.txt"
    -- print $ parse list "[a] a"
    print $ parse datadecls s
    -- mapM_ (putStrLn . show) $ parse datadecl s
