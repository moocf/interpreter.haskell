import Data.List
import System.IO
import Data.Char
import qualified Data.Map as Map


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
  Ida    String  |
  Add    Ast Ast |
  Mul    Ast Ast |
  Sub    Ast Ast |
  Div    Ast Ast |
  Equals Ast Ast |
  IsZero Ast
  deriving (Eq, Read, Show)

type Env = Map.Map String Value


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
run = (eval $ Map.fromList [("a", Numv 1)]) . parse

eval :: Env -> Ast -> Value
eval _ (Numa  x) = Numv  x
eval _ (Boola x) = Boolv x
eval e (Ida x)   = unbind x e
eval e (Add x y) = (eval e x) + (eval e y)
eval e (Mul x y) = (eval e x) * (eval e y)
eval e (Sub x y) = (eval e x) - (eval e y)
eval e (Div x y) = (eval e x) / (eval e y)
eval e (Equals x y) = Boolv $ (eval e x) == (eval e y)
eval e (IsZero x)   = Boolv $ (eval e x) == Numv 0

unbind :: String -> Env -> Value
unbind id e = case v of
    (Just x) -> x
    Nothing  -> error $ "id " ++ id ++ " not set!"
  where v = Map.lookup id e

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
  | isId    t  = "(Ida \""   ++ t ++ "\")"
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

isId :: String -> Bool
isId (c:cs) = isAlpha c && all isAlphaNum cs
