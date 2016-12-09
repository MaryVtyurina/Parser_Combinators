-------------------------------------------------------------------
---------------------------Example---------------------------------
-------------------------------------------------------------------
import Parse_list
import Control.Monad
import Control.Applicative hiding (many)
-- import Data.Char

{-#LANGUAGE MultiParamTypeClasses, MonadComprehensions #-}

datadecls = many_offside datadecl

datadecl = do
                 _  <- symbol "data"
                 x  <- constructor
                 xs <- many variable
                 _  <- symbol "="
                 b  <- condecl `sepby` symbol "|"
                 return (x,xs,b)

                    --      [(x,xs,b) | _  <- symbol "data"
                    --  , x  <- constructor
                    --  , xs <- many variable
                    --  , _  <- symbol "="
                    --  , b  <- condecl `sepby` symbol "|"]


token  :: Parser a -> Parser a
token p = do
            v <- p
            _ <- junk
            return v
--
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

symbol :: String -> Parser String
symbol xs = token (string xs)

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
plus = (+++)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum  = letter `plus` digit

constructor = do
                x  <- upper
                xs <- many alphanum
                token $ return (x, xs)

--parser for identifiers (lower-case letter followed by zero or more alpha-numeric characters)
ident :: Parser String
ident  = do
            x <- lower
            xs <- many alphanum
            return $ x : xs

--keyword check (takes a list of keywords as an argument)
identifier :: [String] -> Parser String
identifier ks = do
                  x <- ident
                  guard $ not (elem x ks)
                  token $ return x
-- --
variable = identifier ["data"]
--
-- condecl = do
--             x  <- constructor
--             ts <- many type2
--             return (x, ts)
--
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
        _ <- open
        x <- p
        _ <- close
        return x
-- for possibly-empty sequences
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep  = (p `sepby1` sep) -- ++ [[]]  -- ????? монада? подойдет ли zero?

--  like many1, but instances of p are separated by a parser sep whose result values are ignored
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
                    x <-p
                    xs <- many f
                    return (x:xs)
                        where
                            f = do
                                    _ <- sep
                                    y <- p
                                    return y

-- parses non empty sequences of items separated by operators that associate to the right, rather than to the left
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = op <*> p <*> chainr1 p op <|> p

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest where
    rest x = do
        f <- op
        y <- p
        rest (f x y) <|> (return x)

-- p `chainr1` op = do
--                     a <- p
--                     rest a
--                     f <- op
--                     y <- p `chainr1` op
--                     return $ (f x y)

data Type = Arrow Type Type -- function
          | Apply Type Type -- Application
          | Var String      -- variable
          | Con String      -- constructor
          | Tuple [Type]    -- Tuple
          | List Type       -- List
          deriving (Show)
-- --

type0 :: Parser Type
type0 = type1 `chainr1` f
                        where
                            f = do
                                 _ <- symbol "->"
                                 return Arrow

type1 = type2 `chainl1` return Apply

type2 = var +++ con +++ list +++ tuple
--
var = variable >>= return . Var


condecl = do
            x  <- constructor
            ts <- many type2
            return (x,ts)

con = do
        x <- constructor
        return $ Con (snd x)

list = bracket (symbol "[") type0 (symbol "]") >>= return . List
tuple = do
            ts <- bracket (symbol "(") (type0 `sepby` symbol ",") (symbol ")")
            return $ f ts
            where
                f [t] = t
                f ts  = Tuple ts

main = do
    a <- readFile "test.txt"
    print $ unS (unR datadecls (1,1)) ((1,1), a)
