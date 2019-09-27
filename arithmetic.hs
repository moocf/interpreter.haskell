import Data.List
import System.IO


data Value =
  Numv  Float |
  Boolv Bool
  deriving (Eq)

instance Show Value where
  show (Numv x)  = show x
  show (Boolv x) = show x

instance Num Value where
  (Numv x) + (Numv y) = Numv $ x + y
  (Numv x) * (Numv y) = Numv $ x * y
  abs (Numv x) = Numv $ abs x
  signum (Numv x) = Numv $ signum x
  fromInteger x = Numv $ fromInteger x
  negate (Numv x) = Numv $ negate x

instance Fractional Value where
  (Numv x) / (Numv y) = Numv $ x / y
  fromRational x = Numv $ fromRational x


data Ast =
  Numa   Float   |
  Boola  Bool    |
  Add    Ast Ast |
  Mul    Ast Ast |
  Sub    Ast Ast |
  Div    Ast Ast |
  Equals Ast Ast |
  IsZero Ast
  deriving (Eq, Read, Show)


main = do
  putStr "arithmetic: "
  hFlush stdout
  exp <- getLine
  if null exp
    then return ()
    else do
      putStrLn (show . run $ exp)
      main

run :: String -> Value
run = eval . parse

eval :: Ast -> Value
eval (Numa  x) = Numv  x
eval (Boola x) = Boolv x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Div x y) = (eval x) / (eval y)
eval (Equals x y) = Boolv $ (eval x) == (eval y)
eval (IsZero x)   = Boolv $ (eval x) == Numv 0

parse :: String -> Ast
parse s = (read . unwords . map token . words $ bpad) :: Ast
  where bpad = replace "(" " ( " . replace ")" " ) " $ s

token :: String -> String
token "+" = "Add"
token "*" = "Mul"
token "-" = "Sub"
token "/" = "Div"
token "=" = "Equals"
token "zero?" = "IsZero"
token t
  | isFloat t  = "(Numa "  ++ t ++ ")"
  | isBool  t  = "(Boola " ++ t ++ ")"
  | otherwise  = t


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace from to all@(x:xs)
  | from `isPrefixOf` all = to ++ (replace from to . drop (length from) $ all)
  | otherwise             = x : replace from to xs

isFloat :: String -> Bool
isFloat s = case (reads s) :: [(Float, String)] of
  [(_, "")] -> True
  _         -> False

isBool :: String -> Bool
isBool s = case (reads s) :: [(Bool, String)] of
  [(_, "")] -> True
  _         -> False
