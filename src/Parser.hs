module Parser where

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

result :: a -> Parser a
result v = Parser $ \input -> [(v, input)]

zero :: Parser a
zero = Parser (const [])

item :: Parser Char
item =
  Parser
    ( \s -> case s of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

seq :: Parser a -> Parser b -> Parser (a, b)
Parser p `seq` Parser q = Parser (\s -> [((a, b), s'') | (a, s') <- p s, (b, s'') <- q s'])

bind :: Parser a -> (a -> Parser b) -> Parser b
(Parser p) `bind` f = Parser (\s -> concat [runParser (f a) s' | (a, s') <- p s])

seq' :: Parser a -> Parser b -> Parser (a, b)
p `seq'` q = p >>= (\x -> q >>= \y -> return (x, y))

instance Functor Parser where
  -- fmap f (Parser g) = Parser $ \s -> fmap (\(x, y) -> (f x, y)) (g s)
  fmap f (Parser g) = Parser (\s -> [(f a, b) | (a, b) <- g s])

instance Applicative Parser where
  pure = result
  (Parser x) <*> (Parser y) =
    Parser
      ( \s ->
          [ (f a, s'') | (f, s') <- x s, (a, s'') <- y s'
          ]
      )

instance Monad Parser where
  (>>=) = bind

sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= (\x -> if pred x then result x else zero)

char :: Char -> Parser Char
char x = sat (x ==)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
Parser p `plus` Parser q = Parser (\s -> (p s) ++ (q s))

letter :: Parser Char
letter = lower `plus` upper
