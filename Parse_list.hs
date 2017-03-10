-- Code from Monadic Parser Combinator
{-#LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, MonadComprehensions, InstanceSigs, ScopedTypeVariables,
            FunctionalDependencies, UndecidableInstances #-}

module Parse_list where

import Control.Monad (Monad, (>>=), (>=>), return, MonadPlus, mzero, mplus, guard, ap)
import Control.Applicative (Applicative, Alternative, empty, (<|>))
import Prelude

---------------------------------------------------------------------------
--                    Type and class for states (transformer form)
---------------------------------------------------------------------------
newtype StateM m s a = StateM { unS :: s -> m (a,s) } deriving Functor

instance Monad m => Applicative (StateM m s) where
    pure :: a -> StateM m s a
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateM m s) where
-- result v :: a -> StateM m s a
    return v = StateM $ \s -> return (v, s)
-- bind :: StateM m s a -> (a -> StateM m s b) -> StateM m s b
    stm >>= f = StateM $
                    unS stm >=>
                        (\(a, s') -> (unS $ f a) s')

instance MonadPlus m => Alternative (StateM m s) where
    empty     = StateM $ const mzero
    s1 <|> s2 = StateM $ \s -> unS s1 s <|> unS s2 s

instance MonadPlus m => MonadPlus (StateM m s) where
    mzero = empty -- StateM $ const
    mplus = (<|>) -- StateM $ \s -> unS s1 s `mplus` unS s2 s

class Monad m => StateMonad m s | m -> s
  where
      update :: (s -> s) -> m s
      set    :: s -> m s
      fetch  :: m s
      set s   = update $ const s
      fetch   = update id

instance Monad m => StateMonad (StateM m s) s where
      -- update :: Monad m => (s -> s) -> StateM m s s
      update f =  StateM $ \s -> return (s, f s)

-- newtype I a = I a
--
-- type State s a = StateM I s a -- non-transformer -- State { unS :: s -> (a,s) }

---------------------------------------------------------------------------
--                    Type and class for readers (transformer form)
---------------------------------------------------------------------------
--ReaderM $ const $ StateM $ const []
newtype ReaderM m s a = ReaderM { unR :: s -> m a } deriving Functor

instance Monad m => Applicative (ReaderM m s) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ReaderM m s) where
    return a  = ReaderM $ const $ return a
    r >>= f   = ReaderM bR where
        bR s = unR r s >>= bM s
        bM s a = unR (f a) s

class Monad m => ReaderMonad m s | m -> s
    where
        env    :: m s
        setenv :: s -> m a -> m a

instance Monad m => ReaderMonad (ReaderM m s) s
    where
        -- env :: ReaderM m s s
        env = ReaderM $ \s -> return s --результат вычислений
        -- setenv :: s -> ReaderM m s a -> ReaderM m s a
        setenv s srm = ReaderM $ \_ -> unR srm s -- замена текущего состояния новым результатом

instance MonadPlus m => Alternative (ReaderM m s) where
    empty     = ReaderM $ const mzero
    r1 <|> r2 = ReaderM $ \s -> unR r1 s <|> unR r2 s

instance StateMonad m a => StateMonad (ReaderM m s) a
    where
        update f   = ReaderM $ \_ -> update f

---------------------------------------------------------------------------
--                               Parser type
---------------------------------------------------------------------------

type Pstring  = (Pos,String)
type Pos      = (Int,Int)

type Parser a = ReaderM (StateM [] Pstring) Pos a

-- Just a friendly reminder:
-- newtype ReaderM m s a = ReaderM (s -> m a)
-- newtype StateM m s a = StateM (s -> m (a,s))
-- ReaderM (StateM [] Pstring) Pos a    ~
--     Pos -> StateM [] Pstring a       ~
--     Pos -> Pstring -> [(a, Pstring)]

---------------------------------------------------------------------------
--                    Basic parser combinators
---------------------------------------------------------------------------
--item successfully consumes the first character if
-- the input string is non-empty, and fails otherwise
item :: Parser Char
item  = do
            (pos, x : _) <- update newstate
            defpos       <- env
            guard $ onside pos defpos
            return x
   --
   -- item :: Parser Char
   -- item  = \inp -> case inp of
   --                    []     -> []
   --                    (x:xs) -> [(x,xs)]
{- -- cf.:
    [(x, pstr)
            | (pos, x : _) <- update newstate
            , defpos       <- env
            , onside pos defpos]
-}

--A position is onside if its column number is strictly greater
--than the current definition column
onside :: Pos -> Pos -> Bool
onside (l,c) (dl,dc) = (c > dc) || (l == dl)

--Takes the first character from the input string,
--and updates the current position
newstate :: Pstring -> Pstring
newstate ((l,c),x:xs)
    = (newpos,xs)
        where
            newpos = case x of
                '\n' -> (l+1,0)
                '\t' -> (l,((c `div` 8)+1)*8)
                _    -> (l,c+1)

--The first result produced by certain parsers
first :: Parser a -> Parser a
first p = ReaderM f where
        arg pos pstr = unS (unR p pos) pstr
        f pos = StateM $ \pstr ->
            -- let
            --     arg = unS (unR p pos) pstr in
                if null $ arg pos pstr then [] else [head $ arg pos pstr]

--        f pos  = upd $ unR p pos
        -- upd sa = do
        --     a <- sa
        --     return a

    --first p =
    --          \inp -> case p inp of
    --                 []     -> []
    --                 (x:xs) -> [x]

(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = first (p <|> q)

--Parser for white-space and comments
string       :: String -> Parser String
string ""     = return ""
string (x:xs) = char x    >>= \_ ->
                   string xs >>= \_ ->
                   return (x:xs)

char  :: Char -> Parser Char
char x = sat (\y -> x == y)

-- bind      :: Parser a -> (a -> Parser b) -> Parser b
-- p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]

result  :: a -> Parser a
result v = ReaderM f where
        f pos = StateM $ \pstr -> [(v, pstr)]

zero :: Parser a
zero = ReaderM f where
        f pos = StateM $ \pstr -> []

sat  :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
   if p x then result x else zero

many  :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

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

comment :: Parser ()
comment = [() | _ <- string "--"
            , _ <- many (sat (\x -> x /= '\n'))]

spaces :: Parser ()
spaces = [() | _ <- many1 (sat isSpace)]
    where
        isSpace x =
            (x == ' ') || (x == '\n') || (x == '\t')

junk :: Parser ()
junk  = setenv (0, -1) (many (spaces +++ comment)) >> return ()

-- Combinator that parses a sequence of definitions subject
-- to the Gofer offside rule
many1_offside  :: Parser a -> Parser [a]
many1_offside p = [vs | (pos, _) <- fetch
                      , vs      <- setenv pos (many1 (off p))]

--Setting the definition position locally for
--each new definition in the sequence
off  :: Parser a -> Parser a
off p = [v | (dl, dc)   <- env
           , ((l, c), _) <- fetch
           , c == dc
           , v         <- setenv (l, dc) p]

--Can also parse an empty sequence of definitions
many_offside :: Parser a -> Parser [a]
many_offside p = many1_offside p +++ return []

--TODO
-- тесты
-- починить many_offside
