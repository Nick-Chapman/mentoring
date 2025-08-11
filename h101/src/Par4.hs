-- | 4-value Parser Combinators
module Par4 (Par,parse,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,skip,separated,terminated,many,some,digit,dot,noError,Position(..),position) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
import qualified Data.Char as Char
import Text.Printf (printf)

instance Functor Par where fmap = liftM
instance Applicative Par where pure = Ret; (<*>) = ap
instance Alternative Par where empty = Fail; (<|>) = Alt
instance Monad Par where (>>=) = Bind

skip :: Par () -> Par ()
separated :: Par () -> Par a -> Par [a]
terminated :: Par () -> Par a -> Par [a]
opt :: Par a -> Par (Maybe a)
alts :: [Par a] -> Par a
word :: Par String
key :: String -> Par ()
int :: Par Int
ws1 :: Par ()
ws0 :: Par ()
digit :: Par Int
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
dot :: Par Char
sat :: (Char -> Bool) -> Par Char
char :: Par Char
noError :: Par a -> Par a
position :: Par Position

skip p = do _ <- many p; return ()
separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
terminated term p = alts [ pure [], do x <- p; term; xs <- terminated term p; pure (x:xs) ]
opt p = alts [ pure Nothing, fmap Just p ]
alts = foldl Alt Fail
word = some $ sat Char.isAlpha
key cs = NoError (mapM_ lit cs)
int = foldl (\acc d -> 10*acc + d) 0 <$> some digit
ws1 = do sp; ws0
ws0 = do _ <- many sp; return ()
digit = digitOfChar <$> sat Char.isDigit
sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
dot = sat (/= '\n')

sat = Satisfy
char = sat (const True)

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

noError = NoError
position = Pos

data Par a where
  Pos :: Par Position
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Satisfy :: (Char -> Bool) -> Par Char
  NoError :: Par a -> Par a
  Alt :: Par a -> Par a -> Par a

type Res a = Either Int (a,Int)

-- Four continuations:
data K4 a b = K4
  { eps :: a -> Res b            -- success; *no* input consumed
  , succ :: Int -> [Char] -> a -> Res b -- success; input consumed
  , fail :: () -> Res b          -- failure; *no* input consumed
  , err :: Int -> [Char] -> Res b       -- failure; input consumed (so an error!)
  }

parse :: FilePath -> Par a -> String -> Either String a
parse filename parStart chars0  = do

  case (run 0 chars0 parStart kFinal) of
    Left i -> Left $ printf "%s: failed to parse: %s" filename (report i)
    Right (a,i) -> do
      if i == length chars0 then Right a else
        Left $ printf "%s: unparsed input from: %s" filename (report i)

  where
    _seeall :: String = printf "\nContents:%s\n%s%s\n" bar chars0 bar
    bar = "------------------------------"
    report :: Int -> String
    report i = item ++ " at " ++ show (mkPosition i) -- ++ _seeall
      where
        item = if i == length chars0 then "<EOF>" else show (chars0 !! i)

    mkPosition :: Int -> Position
    mkPosition p = Position {line,col}
      where
        line :: Int = 1 + length [ () | c <- take p chars0, c == '\n' ]
        col :: Int = length (takeWhile (/= '\n') (reverse (take p chars0)))

    kFinal = K4 { eps = \a -> Right (a,0)
                , succ = \i _ a -> Right (a,i)
                , fail = \() -> Left 0
                , err = \i _ -> Left i
                }

    run :: Int -> [Char] -> Par a -> K4 a b -> Res b
    run i chars par k@K4{eps,succ,fail,err} = case par of

      Pos -> do
        -- It would be more efficient to track line/col directly in the state of the parser
        -- instead of mkPosition recounting newlines passed so far each time a position is required.
        let position = mkPosition i
        eps position

      Ret x -> eps x

      Fail -> fail ()

      Satisfy pred -> do
        case chars of
          [] -> fail ()
          c:chars -> if pred c then succ (i+1) chars c else fail ()

      NoError par -> do
        run i chars par K4 { eps = eps
                           , succ = succ
                           , fail = fail
                           , err = \_ _ -> fail ()
                           }

      Alt p1 p2 -> do
        run i chars p1 K4{ eps = \a1 ->
                             run i chars p2 K4{ eps = \_ -> eps a1 -- left biased
                                              , succ
                                              , fail = \() -> eps a1
                                              , err
                                              }
                         , succ
                         , fail = \() -> run i chars p2 k
                         , err
                         }

      Bind par f -> do
        run i chars par K4{ eps = \a -> run i chars (f a) k
                          , succ = \i chars a ->
                                     run i chars (f a) K4{ eps = \a -> succ i chars a -- consume
                                                         , succ
                                                         , fail = \() -> err i chars -- fail->error
                                                         , err
                                                         }
                          , fail
                          , err
                          }


data Position = Position { line :: Int, col :: Int } deriving (Eq,Ord)

instance Show Position where
  show Position{line,col} = show line ++ "'" ++ show col
