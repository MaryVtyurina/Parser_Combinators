-- Code from Monadic Parser Combinator

{-#LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Prelude hiding ((++), return)

-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------

---------------------------------------------------------------------------
--                    Type and class for states (transformer form)
---------------------------------------------------------------------------

newtype StateM m s a = StateM { unS :: s -> m (a,s) } deriving Functor

instance Monad m => Applicative (StateM m s) where

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

class Monad m => StateMonad m s
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

newtype ReaderM m s a = ReaderM { unR :: s -> m a } deriving Functor

instance Monad m => Applicative (ReaderM m s) where

instance Monad m => Monad (ReaderM m s) where
    return a  = ReaderM $ const $ return a
    r >>= f   = ReaderM bR where
        bR s = unR r s >>= bM s
        bM s a = unR (f a) s

class Monad m => ReaderMonad m s
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
item :: Parser Char
item  = do
            (pos, x : _) <- update newstate
            defpos       <- env
            guard $ onside pos defpos
            return x
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

{-
first  :: Parser a -> Parser a                  --                                   ??????????????????????????????????
first p =
     \inp -> case p inp of
                    []     -> []
                    (x:xs) -> [x]

(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = first (p ++ q)

--Parser for white-space and comments
string       :: String -> Parser String
string ""     = [""]
string (x:xs) = [x:xs | _ <- char x, _ <- string xs]

char  :: Char -> Parser Char
char x = sat (\y -> x == y)

bind      :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]

result  :: a -> Parser a
result v = \inp -> [(v,inp)]

zero :: Parser a
zero = \inp -> []

sat  :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
   if p x then result x else zero

comment :: Parser ()
comment = [() | _ <- string "--"
            , _ <- many (sat (\x -> x /= '\n'))]

spaces :: Parser ()
spaces = [() | _ <- many1 (sat isSpace)]
    where
    isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

junk :: Parser ()
junk  = [() | _ <- setenv (0,-1) (many (spaces +++ comment))]

--Combinator that parses a sequence of definitions subject
-- to the Gofer offside rule
many1_offside  :: Parser a -> Parser [a]
many1_offside p = [vs | (pos,_) <- fetch
                      , vs      <- setenv pos (many1 (off p))]

--Setting the definition position locally for
--each new definition in the sequence
off  :: Parser a -> Parser a
off p = [v | (dl,dc)   <- env
           , ((l,c),_) <- fetch
           , c == dc
           , v         <- setenv (l,dc) p]

--Can also parse an empty sequence of definitions
many_offside :: Parser a -> Parser [a]
many_offside p = many1_offside p +++ [[]]
-}
-------------------------------------------------------------------
---------------------------Example---------------------------------
-------------------------------------------------------------------
--
--
-- -- class Monad m => Monad0Plus m where
-- -- zero :: m a
-- -- (++) :: m a -> m a -> m a
--
-- -- instance Monad0Plus Parser a where
-- -- -- zero :: Parser a
-- -- zero     = \inp -> []
-- -- -- (++) :: Parser a -> Parser a -> Parser a
-- -- p ++ q   = \inp -> (p inp ++ q inp)
--
-- type Data = (String,            -- type name
--             [String],           -- parametrs
--             [(String, [Type])]) -- constructors and arguments
-- datadecls = many_offside datadecl
--
-- datadecl = [(x,xs,b) | _  <- symbol "data"
--                      , x  <- constructor
--                      , xs <- many variable
--                      , _  <- symbol "="
--                      , b  <- condecl `sepby` symbol "|"]
--
-- token  :: Parser a -> Parser a
-- token p = [v | v <- p, _ <- junk]
--
-- symbol :: String -> Parser String
-- symbol xs = token (string xs)
--
-- constructor = token [(x, xs) | x  <- upper
--                              , xs <- many alphanum]
--
-- variable = identifer ["data"]
--
-- condecl = [(x, ts) | x  <- constructor
--                    , ts <- many type2]
--
-- bracket :: Parser a -> Parser b -> Parser c -> Parser b
-- bracket open p close = [x | _ <- open, x <- p, _ <- close]
--
-- sepby :: Parser a -> Parser b -> Parser [a]
-- p `sepby` sep  = (p `sepby1` sep) ++ [[]]
--
-- sepby1 :: Parser a -> Parser b -> Parser [a]
-- p `sepby1` sep = [x:xs|x <-p
--                       , xs <- many [y | _ <- sep, y <- p]]
--
-- data Type = Arrow Type Type -- function
--           | Apply Type Type -- Application
--           | Var String      -- variable
--           | Con String      -- constructor
--           | Tuple [Type]    -- Tuple
--           | List Type       -- List
--
-- first  :: Parser a -> Parser a
-- first p = \inp -> case p inp of
--                     []     -> []
--                     (x:xs) -> [x]
--
-- (+++)  :: Parser a -> Parser a -> Parser a
-- p +++ q = first (p ++ q)
--
-- type0 :: Parser Type
-- type0 = type1 `chainr1` [Arrow | _ <- symbol "->"]
-- type1 = type2 `chainl1` [Apply]
-- type2 = var +++ con +++ list +++ tuple
--
-- var = [Var x | x <-variable]
-- con = [Con x | x <- constructor]
-- list = [List x | x <- bracket (symbol "[")
--                               type0
--                               (symbol "]")]
-- tuple = [f ts | ts <- bracket
--                           (symbol "(")
--                           (type0 `sepby` symbol ",")
--                           (symbol ")")]
--         where f [t] = t
--               f ts  = Tuple ts

main = undefined
